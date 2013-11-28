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
function MedianFilter, im, win

	win = long(win)
	if 2*(win/2) eq win then win=win+1
	w = (win-1)/2

	out = im

	d = size(im,/dimensions)

	for i=w,d[0]-1-w do begin
		for j=w,d[1]-1-w do begin
			out[i,j] = median(im[i-w:i+w,j-w:j+w])
		endfor
	endfor

	return, out
end