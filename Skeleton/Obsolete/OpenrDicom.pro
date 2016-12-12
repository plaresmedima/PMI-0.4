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


function OpenrDicom, file, unit, group, element

	;Opens a Dicom file for reading
	;Returns 0 if the file is not dicom, else returns 1

	;By default, the file pointer is positioned just after the preamble.

	;When the optional arguments 'group' and 'element' are given,
	;the file pointer is placed just after the group tag 'group' and element tag 'element'.
	;When this element does not exist, the file pointer is released and 0 is returned

	on_ioerror, exit

	get_lun, unit
	openr,unit,file
	s = fstat(unit)
	if s.size lt 132 then goto, exit
	point_lun, unit, 128
	preamble = 'DICM'
	readu, unit, preamble
	if preamble ne 'DICM' then goto, exit

	if n_params() eq 2 then return, 1B

	gr = 0US 	& readu, unit, gr
	el = 0US 	& readu, unit, el

	while (gr ne group) or (el ne element) do begin

		vr = '00' & readu, unit, vr
		length = LMU__ReadDicom__length(unit,vr)
		point_lun, -unit, position
		point_lun, unit, position + length

		if eof(unit) then goto, exit

		readu, unit, gr
		readu, unit, el

	endwhile

	return, 1B

	exit:
	free_lun, unit
	return, 0B
end