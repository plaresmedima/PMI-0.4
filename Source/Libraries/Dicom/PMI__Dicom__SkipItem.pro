;Skips an Item in a sequence of items (SQ)

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


pro PMI__Dicom__SkipItem, unit, ts

	PMI__Dicom__ReadTag, unit, gr, el, ts

	big_endian = (gr NE '0002'x) AND (ts EQ '1.2.840.10008.1.2.2')

	length = bytarr(4)
	readu, unit, length
	IF big_endian THEN length = reverse(length,/overwrite)
	length = ulong(length, 0, 1)

	if length eq 'FFFFFFFF'x then begin ;Item with undefined length
		ok = PMI__Dicom__NextTag(unit, gr, el, ts)
		while (gr ne 'FFFE'x) or (el ne 'E00D'x) do begin ;Item delimitation item
			PMI__Dicom__SkipDataElement, unit, ts
			ok = PMI__Dicom__NextTag(unit, gr, el, ts)
		endwhile
		length = 8
	endif

	point_lun, -unit, p
	point_lun, unit, p + length
end