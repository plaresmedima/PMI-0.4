;Reads an Item in a sequence of items (SQ)

;Initially the file pointer is placed before the group tag of the item
;Upon exit, it placed before the next group tag

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



function PMI__Dicom__ReadItem, unit, TransferSyntaxUID=ts, Template=Template

	Item = Obj_New('HEADER')

	PMI__Dicom__ReadTag, unit, gr, el, ts

	length = bytarr(4)
	readu, unit, length
	big_endian = (gr NE '0002'x) AND (ts EQ '1.2.840.10008.1.2.2')
	IF big_endian THEN length = REVERSE(length,/OVERWRITE)
	length = ulong(length, 0, 1)

	if length eq 'FFFFFFFF'x then begin ;Item with undefined length
		ok = PMI__Dicom__NextTag(unit, gr, el, ts)
		while (gr ne 'FFFE'x) or (el ne 'E00D'x) do begin ;Item delimitation item
			Item->set, obj_new('DATA_ELEMENT',unit=unit, TransferSyntaxUID=ts, Template=Template), /append
			ok = PMI__Dicom__NextTag(unit, gr, el, ts)
		endwhile
		point_lun, -unit, p
		point_lun, unit, p + 8	;skip item delimitation item
	endif else begin ;Item with defined length
		point_lun, -unit, pi
		p = pi
		while p lt pi+length do begin
			de = obj_new('DATA_ELEMENT',unit=unit, TransferSyntaxUID=ts, Template=Template)
			Item->set, de, /append
			point_lun, -unit, p
		endwhile
	endelse


	return, Item
end