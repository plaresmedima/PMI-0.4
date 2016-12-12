;Returns the integral of the vector, calculated by
;linear interpolation of the data


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


function IntVector, X, Y

	n = n_elements(Y)
	Z = (Y[1:n-1]+Y[0:n-2]) * (X[1:n-1]-X[0:n-2]) / 2

	Int = dblarr(n)

	Int[1] = Z[0] & for i=2L,n-1 do Int[i] = Int[i-1] + Z[i-1]

;	Int[1:n-1] = total(Z,/cumulative)

	return, Int


end