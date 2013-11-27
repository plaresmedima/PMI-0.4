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


pro PMI__ReadPlot, file, X, Y


	openr, 1, file


	;SKIP X-HEADER


	s=''
	readf, 1, s
	readf, 1, s


	;READ X-ARRAY


	readf, 1, s
	X = [s]
	while strlen(s) gt 0 do begin
		readf, 1, s
		X=[X,s]
	endwhile
	n = n_elements(X)-1
	X = X[0:n-1]


	;SKIP Y-HEADER


	readf, 1, s
	readf, 1, s


	;READ Y-ARRAY


	Y=X
	for i=0L,n-1 do begin
		readf, 1, s
		Y[i] = s
	endfor

	close, 1


	;CONVERT TO FLOATING POINT

	X = float(X)
	Y = float(Y)
end