;MODEL PARAMETERS

;P = [V, FP, v, E]

;DEFINED AS FOLLOWS

;V = VP+VE	(Total Extracellular Volume)
;FP (Plasma Flow)
;v = VE/(VP+VE)  (Extravascular Volume Fraction)
;E = FE/FP  (Extraction Fraction)

;NOTE: in the kidney (with outwards reabsorption - Sourbron IR 2008),
;	FE is the tubular flow FT,
;	and VE is the APPARENT tubular volume VT/(1-f),
;	with VT the tubular volume and f the reabsorption fraction
;	Hence V is not necessarily < 1

;FITTED FUNCTION

;C(t) = FP exp(-t/TP)*Ca(t) +  (FE/TP) step(-t/TE) * exp(-t/TP)*Ca(t)

;NOTE: For filtration compartment replace "step" by "exp"

;FIT PARAMETERS

;FP, TP, FE, TE

;DEFINED AS FOLLOWS IN TERMS OF THE MODEL PARAMETERS

;TP = VP/FP = V (1-v) / FP
;FE = E FP
;TE = VE/FE = V v / FE

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


Pro SingleInletFiltrationPlugFlow, X, P, C, C_DER

	if n_params() eq 0 then return

	ni=X[0] & n=n_elements(X[ni+1:*])/2
	ti=X[1:ni] & time=X[ni+1:ni+n] & input=X[ni+n+1:*]

	TP = P[0]*(1-P[2])/P[1]
	FE = P[3]*P[1]
	TE = P[0]*P[2]/FE

	conv1 = ExpConvolution(1/TP,[time,input]) ;exp(-t/TP) * Ca(t)
	conv2 = StepConvolution(1/TE,[time,conv1]) ;step(-t/TE) * exp(-t/TP) * Ca(t)

	C = P[1]*conv1[ti] + (FE/TP)*conv2[ti]

	IF n_params() LT 4 THEN return

	;Derivatives wrt model parameters

end