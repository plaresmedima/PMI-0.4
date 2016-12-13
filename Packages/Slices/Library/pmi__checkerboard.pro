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
function PMI__Checkerboard, im1, im2, w

	d = size(im1,/dimensions)
	n = floor(d[1:2]/w)

	im=im2

	for i=0L,n[0]-1,2 do begin
	for j=0L,n[1]-1,2 do begin
		im[*,i*w:(i+1)*w-1,j*w:(j+1)*w-1] = im1[*,i*w:(i+1)*w-1,j*w:(j+1)*w-1]
	endfor
	endfor

	for i=1L,n[0]-1,2 do begin
	for j=1L,n[1]-1,2 do begin
		im[*,i*w:(i+1)*w-1,j*w:(j+1)*w-1] = im1[*,i*w:(i+1)*w-1,j*w:(j+1)*w-1]
	endfor
	endfor

	return, im
end