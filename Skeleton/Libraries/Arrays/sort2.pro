;sort on x, then on y
;return: vector of sorted indices

;x and y must have the same number of elements



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


function sort2, x, y, n=n

	s = sort(x)

	xs = x[s]
	ri = uniq(xs)
	rx = xs[ri]
	ri = [-1,ri]
	nx = n_elements(rx)
	n = lonarr(nx)

	for i=1L,nx do begin
		n[i-1] = ri[i]-ri[i-1]
		si = s[1+ri[i-1]:ri[i]]
		s[1+ri[i-1]:ri[i]] = si[sort(y[si])]
	endfor

	return, s




	;OLD VERSION WITH reduce()

;	s = sort(x)
;
;	rx = reduce(x[s],ri,n=nx)
;	ri = [ri,n_elements(x)]
;	n = lonarr(nx)
;
;	for i=0L,nx-1 do begin
;		n[i] = ri[i+1]-ri[i]
;		si = s[ri[i]:ri[i+1]-1]
;		s[ri[i]:ri[i+1]-1] = si[sort(y[si])]
;	endfor
;
;	return, s
end