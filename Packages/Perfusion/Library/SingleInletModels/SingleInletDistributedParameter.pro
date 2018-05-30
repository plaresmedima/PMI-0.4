;MODEL PARAMETERS

;P = [V, F, v, f]

;DEFINED AS FOLLOWS

;V = VP+VE	(Total Extracellular Volume)
;F (Plasma Flow)
;v = VE/(VP+VE)  (Extravascular Volume Fraction)
;f = PS/(PS+FP)  (Exchange Fraction)

;FITTED FUNCTION

;C(t) = FP Amin exp(-tKmin)*Ca(t) + FP (1-Amin) exp(-tKplus)*Ca(t)

;REPLACE BY:

;C(t) = FP R(t) * Ca(t)

;with R(t) as defined in Sourbron Buckley 2012





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
Pro SingleInletDistributedParameter, X, P, C, C_DER

	if n_params() eq 0 then return

	ni=X[0] & n=n_elements(X[ni+1:*])/2
	ti=X[1:ni] & time=X[ni+1:ni+n] & input=X[ni+n+1:*]

	A = P[2]-P[2]*P[3]+P[3]
	B = P[2]*(1-P[2])*P[3]*(1-P[3])

	ts = sqrt(1-4*B/A^2)
	ta = 0.5*A/P[3]

	tp = ta*(1+ts)
	tm = ta*(1-ts)

	Kp = P[1]/(P[0]*tm)
	Km = P[1]/(P[0]*tp)
	Am = (1-tm)/(tp-tm)

	cP = ExpConvolution(Kp,[time,input],Der=dcP)
	cM = ExpConvolution(Km,[time,input],Der=dcM)

	cP=cP[ti] & cM=cM[ti]

	C = P[1]*(1-Am)*cP + P[1]*Am*cM


end