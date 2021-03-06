;MODEL PARAMETERS

;P = [FA, FV, VE, EI, EB]

;DEFINED AS FOLLOWS

;EI = KI/(FA+FV+KI)	(Intracellular Uptake Fraction)
;EB = KB/(FA+FV+KB)	(Biliary Excretion Fraction)

;FITTED FUNCTION

;Jin(t) = FA ca(t) + FV cv(t)
;C(t) = (1-A) exp(-t/TE) * Jin(t) + A exp(-t/TI) * Jin(t)

;FIT PARAMETERS

;FA, FV, A, TE, TI

;DEFINED AS FOLLOWS

;KI = (FA+FV) EI/(1-EI)
;KB = (FA+FV) EB/(1-EB)
;
;TE = VE / (FA+FV+KI)
;TI = (1-VE) / KB
;
;A = (KI/VE) TI TE / (TI-TE)
;
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


Pro DualInletFiltration, X, P, C, Ci

  if n_params() eq 0 then return

	ni=X[0] & n=n_elements(X[ni+1:*])/3
	ti=X[1:ni] & time=X[ni+1:ni+n]
	cA=X[ni+n+1:ni+2*n] & cV=X[ni+2*n+1:*]

  ;P = [FA, FV, VE, EI, EB]

	FA = P[0]
	FV = P[1]
	VE = P[2]
	EI = P[3]
	EB = P[4]

	KI = (FA+FV) * EI/(1-EI)
	KB = (FA+FV) * EB/(1-EB)
	MTTE = VE / (FA+FV+KI)
	MTTI = (1-VE) / KB
	AMPL = (KI/VE) * MTTI*MTTE / (MTTI-MTTE)

	Jin = FA*cA + FV*cV

	convE = ExpConvolution(1/MTTE,[time,Jin])
	convI = ExpConvolution(1/MTTI,[time,Jin])

	C = (1-AMPL)*convE[ti] + AMPL*convI[ti]

	IF n_params() LT 4 THEN return

	;Ci = analytical pder's not implemented

	end