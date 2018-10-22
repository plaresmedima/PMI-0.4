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


Pro DualInletUptake, X, P, F, Fi

	IF n_params() EQ 0 THEN RETURN

	;P = [FA,FV,EV,E]

	KI = (P[0]+P[1])*P[3]/(1-P[3])
	KT = (P[0]+P[1]+KI)/P[2]

	ni=X[0] & n=n_elements(X[ni+1:*])/3
	ti=X[1:ni] & time=X[ni+1:ni+n]
	cA=X[ni+n+1:ni+2*n] & cV=X[ni+2*n+1:*]

	Jin = P[0]*cA + P[1]*cV

	cE 	= ExpConvolution(KT,[time,Jin])/P[2]
	CH 	= IntVector(time,cE)*KI

	F = P[2]*cE[ti] + CH[ti]

	IF n_params() LT 4 THEN RETURN

end