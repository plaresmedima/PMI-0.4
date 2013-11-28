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


function addstrip, im, max=max

	if n_elements(max) eq 0 then max=max(im)

	d = size(im,/dimensions)
	y = d[1]
	x = floor(d[0]/20E)

	clr 	= max*findgen(y)/(y-1)
	imstrip = make_array(d[0]+x,y,type=size(im,/type))

	imstrip[0:d[0]-1,*] = im
	for i=0L,y-1 do imstrip[d[0]:*,i] = clr[i]

	return, imstrip
end