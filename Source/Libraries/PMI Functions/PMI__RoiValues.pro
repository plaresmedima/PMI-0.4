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


FUNCTION PMI__RoiValues, Path, S, R, id, z=z, t=t, cnt=cnt

	if  	(n_elements(z) ne 0) $
	and 	(n_elements(t) ne 0) $
	then 	return, PMI__RoiSliceValues(Path,S,R,z,t,cnt=cnt)

	if  	(n_elements(t) ne 0) $
	then 	return, PMI__RoiVolumeValues(Path,S,R,t,cnt=cnt)

	d = R->d()
	z = R->z()
	t = R->t()

	data = [0E]

	for k=0L,d[2]*d[3]-1 do begin

		PMI__Message, id, 'Loading data of ' + R->name(), k/(d[2]*d[3]-1E)

		p = reform_ind(d[2:3],ind=k)
		v = PMI__RoiSliceValues(Path,S,R,z[p[0]],t[p[1]],cnt=cnt)
		if cnt gt 0 then data = [data,v]
	endfor

	PMI__Message, id

	cnt = n_elements(data)-1
	if cnt gt 0 then return, data[1:*]

	return, 0B
END