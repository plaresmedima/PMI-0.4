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


function PMI__Circle, r, c, n=n

	if n_elements(n) eq 0 then n = 360L
	if n_elements(c) eq 0 then c = [0L,0L]

	xy = dblarr(n,2)
	a = 2*acos(-1D)*dindgen(n)/(n-1D)

	for i=0L,n-1 do begin
		xy[i,0] = c[0] + r*cos(a[i])
		xy[i,1] = c[1] + r*sin(a[i])
	endfor

	return, xy
end