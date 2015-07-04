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


function PMI__Dicom__NextTag, unit, gr, el, ts

	if eof(unit) then begin
		point_lun, unit, 128
	    preamble = 'DICM'
	    readu, unit, preamble
	    if preamble ne 'DICM' then point_lun, unit, 0
		return_value = 0B
	endif else return_value = 1B

	PMI__Dicom__ReadTag, unit, gr, el, ts

	point_lun, -unit, p
	point_lun, unit, p - 4

	return, return_value
end