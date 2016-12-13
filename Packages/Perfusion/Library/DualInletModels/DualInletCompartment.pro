;
;
;    Copyright (C) 2012 Steven Sourbron
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



;P[0] = Arterial Inflow
;P[1] = Venous Inflow
;P[2] = Extracelular Volume

Pro DualInletCompartment, X, P, F, Fi

	if n_params() eq 0 then return

	;P = [FA,FV,EV]

	KT = (P[0]+P[1])/P[2]

	ni=X[0] & n=n_elements(X[ni+1:*])/3
	ti=X[1:ni] & time=X[ni+1:ni+n]
	inputA=X[ni+n+1:ni+2*n] & inputV=X[ni+2*n+1:*]

	F0 = ExpConvolution(KT,[time,inputA],Der=D0)
	F1 = ExpConvolution(KT,[time,inputV],Der=D1)

	F0=F0[ti] & F1=F1[ti]
	D0=D0[ti] & D1=D1[ti]

	F = P[0]*F0 + P[1]*F1

	IF n_params() LT 4 THEN RETURN

;	F2 	= P[0]*D0 + P[1]*D1
;	Fi = [[F0],[F1],[F2]]
end