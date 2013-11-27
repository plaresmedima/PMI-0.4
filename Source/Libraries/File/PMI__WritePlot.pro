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


pro PMI__WritePlot, file, X, Y, Xname=Xname, Yname=Yname

	if n_elements(Xname) eq 0 then Xname = 'X-values'
	if n_elements(Yname) eq 0 then Yname = 'Y-values'

	n = n_elements(X)

	openw, 1, file
	printf, 1, Xname
	printf, 1, ''
	for i=0L,n-1 do printf, 1, X[i]
	printf, 1, ''
	printf, 1, Yname
	printf, 1, ''
	for i=0L,n-1 do printf, 1, Y[i]
	close, 1
end