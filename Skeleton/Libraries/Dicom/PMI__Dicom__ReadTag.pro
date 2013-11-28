;Reads the next groups and elements tag
;File pointer must be placed immediately before the group field

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


PRO PMI__Dicom__ReadTag, unit, gr, el, ts

;;;;Read group, element tags,

	gr = bytarr(2) & readu, unit, gr
	el = bytarr(2) & readu, unit, el

	big_endian = ts EQ '1.2.840.10008.1.2.2'
	if uint(gr,0,1) EQ '0002'x then big_endian=0B

	if big_endian then begin
		gr = reverse(gr,/overwrite)
		el = reverse(el,/overwrite)
	endif

	gr = uint(gr,0,1)
	el = uint(el,0,1)
END