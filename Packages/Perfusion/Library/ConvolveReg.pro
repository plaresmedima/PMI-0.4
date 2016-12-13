;
;
;    Copyright (C) 2009 Steven Sourbron
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
;    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
;
;
;
function ConvolveReg, dt, f, g

	n = n_elements(f)

	gp = fltarr(n)
	gm = fltarr(n)
	gp[0:n-2] = (2*g[0:n-2]+g[1:n-1])/6
	gm[1:n-1] = (2*g[1:n-1]+g[0:n-2])/6
	gpm = gp + gm

	c = fltarr(n)
	c[1:n-1] = f[1:n-1] * gp[0] + f[0] * gm[1:n-1]
	for k=2L,n-1 do c[k:n-1]=c[k:n-1] + f[1:n-k]*gpm[k-1]

	return, dt*c
end