;PURPOSE:

	;Reads a single data element

;CALLING SEQUENCE:

	;exists = PMI__Dicom__ReadDataElement(unit, value=value, gr=gr, el=el, vr=vr, Template=Template)
	;Read the next data element

	;OR

	;exists = PMI__Dicom__ReadDataElement(unit, group, element, value=value, gr=gr, el=el, vr=vr, Template=Template)
	;Reads the data element with tags group and element

;ARGUMENTS:

	;unit: LUN of a DICOM file open for reading
	;group, element: Group and element tag of the data element to find
	;Implicit VR: keyword, specifies if the transfer syntax uses implicit or explicit VR


;RETURN VALUES

	;Returns 0 if the data element with tags "group" and "element" does not exist
	;Returns 0 if the data element read is corrupted (ie. length field does not agree with value length)
	;Returns 0 if the data element read has zero length
	;Returns 1 otherwise

	;The value of the data element is returned in the named variable "value".
	;If the data element does not exist, or when the length is 0, then value = 0B.
	;The value of group, element, value representation are return in gr, el, vr

;FILE POINTER POSITIONS

	;The file pointer must be positioned just before a group tag
	;Upon exit, the file pointer is positioned before the next data element

;WRITTEN BY: Steven Sourbron
;DATE: 03-feb-2011





;
;    Copyright (C) 2013 Steven Sourbron
;
;    This program is free software; you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation; either version 2 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License along
;    with this program; if not, write to the Free Software Foundation, Inc.,
;    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.





function PMI__Dicom__ReadDataElement, unit, group, element, value=value, gr=gr, el=el, vr=vr, length=length, TransferSyntaxUID=ts, Template=Template

	vr = 'UN'
	value = 0B

;;;;If a specific group and element tag is given, then forward to that first
;;;;If it doesn't exist, return, 0

	if n_elements(group) ne 0 then $
		if not PMI__Dicom__Point(unit, group, element, ts) then return, 0B

;;;;Read tags, value representation and value length fields

	PMI__Dicom__ReadTag, unit, gr, el, ts
	explicit_vr = (gr EQ '0002'x) OR (TS NE '1.2.840.10008.1.2')
	IF explicit_vr THEN BEGIN
		vr = PMI__Dicom__ReadValueRepresentation(unit)
		valid_vr = total(vr EQ ['FL','FD','SL','SS','UL','US','AT','OB','OF','OW','DS','IS','TM','AE','AS','CS','DA','DT','LO','LT','PN','SH','ST','UI','UT','UN','SQ'],/PRESERVE_TYPE)
		IF NOT valid_vr THEN BEGIN ;if invalid vr then assume implicit vr
			Hdr = LMU__DicomTemplate()
			vr = Hdr->GetVr(gr,el)
			obj_destroy, Hdr
			point_lun, -unit, p
			point_lun, unit, p-2
			length = 0UL
			readu, unit, length
		ENDIF ELSE BEGIN
		    if (gr EQ '7FE0'x) and (el EQ '0010'x) then vr = 'OW' ; Assume pixel data are always 'OW'. Exception built in on 15/06/2012, based on Philips data (Bordeaux). Header wrongly stated 'OB'.
		    length = PMI__Dicom__ReadLength(unit, gr, ts, vr)
		ENDELSE
	ENDIF ELSE BEGIN
		vr = Template->GetVr(gr,el)
		length = PMI__Dicom__ReadLength(unit, gr, ts)
	ENDELSE




;;;;If the length is zero, or longer than the nr. of remaining bytes,
;;;;Then the value is 0 and 0 returned

	if length eq 0 then return, 0B

	s = fstat(unit)
	if vr eq 'SQ' then begin
		value = PMI__Dicom__ReadSequence(unit, TransferSyntaxUID=ts, Template=Template, eof=eof)
		if eof then point_lun, unit, s.size
		return, 1B
	endif

;;;;If the length longer than the nr. of remaining bytes,
;;;;Then the value is 0 and 0 returned

	point_lun, -unit, p
	if s.size - p lt length then begin
		point_lun, unit, 132
		return, 0B
	endif

;;;;Read Value field

	value = bytarr(length)
	readu, unit, value
	value = PMI__Dicom__FormatValue(value, vr, length, big_endian = (ts EQ '1.2.840.10008.1.2.2') AND (gr NE '0002'x))

	return, 1B
end