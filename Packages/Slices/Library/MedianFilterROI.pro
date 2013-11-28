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
function MedianFilterROI, im, ind, win

	win = long(win)
	if 2*(win/2) eq win then win=win+1
	w = (win-1)/2

	out = im

	n = n_elements(ind)
	d = size(im,/dimensions)

	for k=0L,n-1 do begin
		r = reform_ind(d,ind=ind[k])
		x0 = r[0]-w & if x0 lt 0 then x0=0
		x1 = r[0]+w & if x1 ge d[0] then x1=d[0]-1
		y0 = r[1]-w & if y0 lt 0 then y0=0
		y1 = r[1]+w & if y1 ge d[1] then y1=d[1]-1
		out[ind[k]] = median(im[x0:x1,y0:y1])
	endfor

	return, out
end