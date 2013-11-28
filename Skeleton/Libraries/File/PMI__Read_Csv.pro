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


;Reads a file in comma-separated-value format into a 1- or 2-dimensional string array

function PMI__Read_Csv, file, array

	openr, 1, file, error=err

	if err ne 0 then begin
		error = -1
		close, 1
		return, 0B
	endif

	s = fstat(1)
	list = bytarr(s.size)
	readu, 1, list
	close, 1

	list = strsplit(string(list), string([13B,10B]), /extract, /regex, count=rows)
	cols = 0L
	for i=0L,rows-1 do begin
		split = strsplit(list[i], ',', count=rowi)
		cols = max([cols,rowi])
	endfor

	array = strarr(cols, rows)
	for i=0L,rows-1 do begin
		split = strsplit(list[i], ',', count=rowi, /extract)
		if rowi GT 0 then array[0:rowi-1,i] = split
	endfor
	array = reform(array)

	return, 1B
end