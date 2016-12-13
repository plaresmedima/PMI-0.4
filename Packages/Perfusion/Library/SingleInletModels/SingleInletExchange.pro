;MODEL PARAMETERS

;P = [V, F, v, f]

;DEFINED AS FOLLOWS

;V = VP+VE	(Total Extracellular Volume)
;F (Plasma Flow)
;v = VE/(VP+VE)  (Extravascular Volume Fraction)
;f = FE/(FE+FP)  (Exchange Fraction)

;FITTED FUNCTION

;C(t) = FP Amin exp(-tKmin)*Ca(t) + FP (1-Amin) exp(-tKplus)*Ca(t)

;FIT PARAMETERS

;FP, Amin, Kmin, Kplus

;DEFINED AS FOLLOWS IN TERMS OF THE MODEL PARAMETERS (see paper)



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
Pro SingleInletExchange, X, P, C, C_DER

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

	IF n_params() LT 4 THEN return

	;Derivatives wrt model parameters

	A2 = 1-P[3]
	A3 = 1-P[2]

	B2 = (1-2*P[2])*P[3]*(1-P[3])
	B3 = P[2]*(1-P[2])*(1-2*P[3])

	ta2 = 0.5*A2/P[3]
	ta3 = 0.5*A3/P[3] - 0.5*A/P[3]^2

	ts2 = (-2/ts)*(B2-2*B*A2/A)/A^2
	ts3 = (-2/ts)*(B3-2*B*A3/A)/A^2

	tp2 = ta2 + ta2*ts + ta*ts2
	tp3 = ta3 + ta3*ts + ta*ts3

	tm2 = ta2 - ta2*ts - ta*ts2
	tm3 = ta3 - ta3*ts - ta*ts3

	Am2 = (tm2*(1-tp) - tp2*(1-tm))/(tp-tm)^2
	Am3 = (tm3*(1-tp) - tp3*(1-tm))/(tp-tm)^2

	Kp0 = -P[1]/(P[0]^2*tm)
	Kp1 = 1/(P[0]*tm)
	Kp2 = -tm2*P[1]/(P[0]*tm^2)
	Kp3 = -tm3*P[1]/(P[0]*tm^2)

	Km0 = -P[1]/(P[0]^2*tp)
	Km1 = 1/(P[0]*tp)
	Km2 = -tp2*P[1]/(P[0]*tp^2)
	Km3 = -tp3*P[1]/(P[0]*tp^2)

	dcP=dcP[ti]
	dcM=dcM[ti]

	C0 = P[1]*((1-Am)*dcP*Kp0 + Am*dcM*Km0)
	C1 = P[1]*((1-Am)*dcP*Kp1 + Am*dcM*Km1) + C/P[1]
	C2 = P[1]*((-Am2*cP + (1-Am)*dcP*Kp2 + Am2*cM + Am*dcM*Km2))
	C3 = P[1]*((-Am3*cP + (1-Am)*dcP*Kp3 + Am3*cM + Am*dcM*Km3))

	C_DER = [[C0],[C1],[C2],[C3]]
end