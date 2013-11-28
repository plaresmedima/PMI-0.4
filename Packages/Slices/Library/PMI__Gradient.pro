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
function PMI__Gradient, im

	n = size(im, /dimensions)
	grad = im

	for i=0L, n[0]-2 do begin
	for j=0L, n[1]-2 do begin
		grad[i,j] = sqrt((im[i+1,j]-im[i,j])^2 + (im[i,j+1]-im[i,j])^2)
	endfor
	endfor

	grad[n[0]-1,*] = grad[n[0]-2,*]
	grad[*,n[1]-1] = grad[*,n[1]-2]

	return, grad
end