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


FUNCTION LMU__ReadDicom, file, group, element, vr=vr, error=error

	n = n_elements(file)
	if n gt 1 then begin
		v0 = LMU__ReadDicom(file[0],group,element,vr=vr,error=error)
		if error then return, bytarr(n)
		value = make_array(n,type=size(v0,/type))
		value[0] = v0
		for i=1L,n-1 do value[i] = LMU__ReadDicom(file[i],group,element)
		return, value
	endif

	if not OpenrDicom(file,unit,group,element) then goto, exit

	vr = '00' & readu, unit, vr
	length = LMU__ReadDicom__length(unit,vr)

	if length eq 0 then goto, exit

	if not LMU__ReadDicom__ValidVR(vr) then vr = LMU__ReadDicom__GetVR(group,element)
	value = LMU__ReadDicom__value(unit,vr,length)

	error = 0B
	free_lun, unit
	return, value

	exit:

	error = 1B
	free_lun, unit
	return, 0B
END