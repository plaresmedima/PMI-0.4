;C(t) = F exp(-t/T)*Ca(t)
;P = [VP+VE, F]

;T =(VP+VE)/F


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

Pro SingleInletCompartment, X, P, C, C_DER

	if n_params() eq 0 then return

	ni=X[0] & n=n_elements(X[ni+1:*])/2
	ti=X[1:ni] & time=X[ni+1:ni+n] & input=X[ni+n+1:*]

	K = P[1]/P[0] ; F/V

	Conv = ExpConvolution(K,[time,input],Der=dConv)

	C = P[1]*Conv[ti]

	IF n_params() LT 4 THEN return

	;Derivatives wrt model parameters

	K_DER0 = -P[1]/P[0]^2
	K_DER1 = 1/P[0]

	C_DER0 = P[1]*dConv[ti]*K_DER0
	C_DER1 = Conv[ti] + P[1]*dConv[ti]*K_DER1

	C_DER = [[C_DER0],[C_DER1]]
end