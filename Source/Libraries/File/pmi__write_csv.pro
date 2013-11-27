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


;Writes a 1- or 2-dimensional string array to file in comma-separated-value format

pro PMI__write_csv, file, array

	Openw, 1, file

	n = size(array,/n_dimensions)
	d = size(array,/dimensions)

	if n eq 1 then begin
		for i=0L,d[0]-1 do printf, 1, array[i]
	endif else begin
		for i=0L,d[1]-1 do begin
			str = array[0,i]
			for j=1L, d[0]-1 do str=str + ','+array[j,i]
			printf, 1, str
		endfor
	endelse

	Close, 1
end