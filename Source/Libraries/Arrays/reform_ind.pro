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


function reform_ind, D, vec=vec, ind=ind

	D = long(D)
	nD = n_elements(D)

	if n_elements(vec) ne 0 then begin
		v = long(vec)
		ind = 0L & vol = 1L
		for i=0L,nD-1 do begin
			ind = ind + v[i]*vol
			vol = vol * D[i]
		endfor
		return, ind
	endif

	if n_elements(ind) ne 0 then begin
		index = long(ind)
		vec = lonarr(nD)
		for i=0L,nD-1 do begin
		   tmp = index/D[i]
		   vec[i] = index - tmp*D[i]
		   index = tmp
		endfor
		return, vec
	endif
end