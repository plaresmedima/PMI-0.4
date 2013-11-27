;Skips a data element

;Initially the file pointer should be placed before the group tag
;Upon exit, it is placed before the next group tag

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


pro PMI__Dicom__SkipDataElement, unit, ts

	PMI__Dicom__ReadTag, unit, gr, el, ts
	explicit_vr = (gr EQ '0002'x) OR (ts NE '1.2.840.10008.1.2')
	IF explicit_vr THEN BEGIN
		vr = PMI__Dicom__ReadValueRepresentation(unit)
		valid_vr = total(vr EQ ['FL','FD','SL','SS','UL','US','AT','OB','OF','OW','DS','IS','TM','AE','AS','CS','DA','DT','LO','LT','PN','SH','ST','UI','UT','UN','SQ'],/PRESERVE_TYPE)
		IF NOT valid_vr THEN BEGIN ;if invalid vr then assume implicit vr
			point_lun, -unit, p
			point_lun, unit, p-2
			length = 0UL
			readu, unit, length
		ENDIF ELSE length = PMI__Dicom__ReadLength(unit, gr, ts, vr)
	ENDIF ELSE length = PMI__Dicom__ReadLength(unit, gr, ts)


	;Skip SQ with undefined length
	;NOTE: OW or OB can also have umlimited length: NOT YET IMPLEMENTED

	if length eq 'FFFFFFFF'x then begin
		ok = PMI__Dicom__NextTag(unit, gr, el, ts)
		while (gr eq 'FFFE'x) and (el eq 'E000'x) do begin
			PMI__Dicom__SkipItem, unit, ts
			ok = PMI__Dicom__NextTag(unit, gr, el, ts)
		endwhile
		length = 8
	endif

	;Skip Remaining

	point_lun, -unit, p
	point_lun, unit, p+length
end


