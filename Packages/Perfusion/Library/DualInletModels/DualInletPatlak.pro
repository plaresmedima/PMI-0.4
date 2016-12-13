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




;Ci(t) = AF Ca(t) + (1-AF) Cv(t)
;C(t) = EV Ci(t) + Ki * Ci(t)

;P = [AF,EV,KI] ;[Arterial Flow Fraction, Extracellular Volume, Intracellular Uptake Rate]

Pro DualInletPatlak, X, P, F, Fi

	IF n_params() EQ 0 THEN RETURN

	ni=X[0] & n=n_elements(X[ni+1:*])/3
	ti=X[1:ni] & time=X[ni+1:ni+n]
	inputA=X[ni+n+1:ni+2*n] & inputV=X[ni+2*n+1:*]

	Input = P[0]*inputA + (1-P[0])*inputV
	Integral = IntVector(time,input)

	F = P[1]*Input[ti] + P[2]*Integral[ti]

	IF n_params() LT 4 THEN RETURN

end