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


function LMU__ReadDicom__length, unit, vr

	if not LMU__ReadDicom__ValidVR(vr) then begin
		point_lun, -unit, position
		point_lun, unit, position-2
		length = 0UL
	endif else begin
		length = 0US
		if (vr eq 'OB') $
		or (vr eq 'OW') $
		or (vr eq 'OF') $
		or (vr eq 'SQ') $
		or (vr eq 'UT') $
		or (vr eq 'UN') $
		then begin
			readu, unit, length
			length = 0UL
		endif
	endelse

	readu, unit, length

	if length ne 'FFFFFFFF'x then return, length


	;If length=undefined, find the sequence delimitation item and determine length

	point_lun, -unit, pi
	gr = 0US
	el = 0US
	length = 0UL
	while (gr ne 'FFFE'x) or (el ne 'E0DD'x) or (length ne 0) do begin
		gr = el
		readu, unit, el
		point_lun, -unit, position
		readu, unit, length
		point_lun, unit, position
	endwhile
	readu, unit, length
	point_lun, -unit, pf
	point_lun, unit, pi

	return, pf - pi
end