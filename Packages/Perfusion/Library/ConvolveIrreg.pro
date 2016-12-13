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

function ConvolveIrreg, t, g, h

	n = n_elements(t)

	n_int = n*(n+1)/2
	g_int = fltarr(n_int)
	i0=0
	for i=1L,n-1 do begin
		g_int[i0:i0+i-1] = t[i]-t[0:i-1]
		i0 = i0+i
	endfor
	g_int = interpol(g,t,g_int)

	f = fltarr(n)
	i0=0
	for i=1L,n-1 do begin
		f[i] = total(g_int[i0:i0+i-1]*h[0:i-1]*(t[1:i]-t[0:i-1]))
		i0=i0+i
	endfor

	return, f
end