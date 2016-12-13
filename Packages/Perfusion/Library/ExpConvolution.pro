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


function ExpConvolution, l, X, Der=DY

	n = n_elements(X)/2
	Y = dblarr(n)

	if finite(l) eq 0 then begin	;Bug corrected on 25/06/2012 (SPS)
		if arg_present(DY) then DY=Y
		return, Y
	endif

	T = X[0:n-1]
	A = X[n:*]

	DT = T[1:n-1]-T[0:n-2]
	DA = A[1:n-1]-A[0:n-2]

	Z = l*DT

	E = exp(-Z)
	E0 = 1-E
	E1 = Z-E0

	Il = (A[0:n-2]*E0 + DA*E1/Z)/l

	for i=0L,n-2 do Y[i+1] = E[i]*Y[i] + Il[i]

	if not arg_present(DY) then return, Y

	E2 = Z^2-2*E1

    DIl = -DT*Il + (A[0:n-2]*E1 + DA*E2/Z )/(l^2)
    DIl = -E*DT*Y + DIl

	DY = dblarr(n)

	for i=0L,n-2 do DY[i+1] = E[i]*DY[i] + DIl[i]

	return, Y
end
