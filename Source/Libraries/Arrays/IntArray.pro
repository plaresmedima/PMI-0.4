;Returns the integral of the array Y, calculated by
;linear interpolation of the data
;The array must be twodimensional, the integration is
;performed along the second dimension
;X is supposed to be an vector of length n,
;Y is supposed to be an array with n_elements(Y[0,*])==n
;return value is an Array with 1 dimension
;________________________________________________________



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

function IntArray, X, Y

	n = n_elements(X)
	if n_elements(Y[0,*]) ne n then return, -1

	YSum 		= (Y[*,0:n-2]+Y[*,1:n-1])/2.
	TimeDelta 	= X[1:n-1] - X[0:n-2]

	return, YSum # TimeDelta
end