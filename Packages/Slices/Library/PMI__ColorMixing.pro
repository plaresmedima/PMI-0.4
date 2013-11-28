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
function PMI__ColorMixing, im1, im2, C1, C2

	d = size(im1,/dimensions)
	im = bytarr(3,d[0],d[1])

	for i=0L,d[0]-1 do begin
	for j=0L,d[1]-1 do begin
		im[*,i,j] = (im1[i,j]*C1 + im2[i,j]*C2)/(im1[i,j] + im2[i,j])
	endfor
	endfor

	return, im
end