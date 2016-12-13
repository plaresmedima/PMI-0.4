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



Pro DualInletExchangeUptake, X, P, F, Fi

	IF n_params() EQ 0 THEN RETURN

	;P = [FA,FV,VE+VP,EX,VP/(VE+VP),EI]

	FE = (P[0]+P[1])*P[3]/(1-P[3])
	VP = P[2]*P[4]
	VE = P[2]-VP
	FI = FE*P[5]/(1-P[5])

	Q = [P[0],P[1],FE,VP,VE,FI]

	;P = [FA,FV,FE,VP,VE,FI]

	FP = Q[0]+Q[1]
	KP = (FP+Q[2])/Q[3]
	KE = Q[2]/Q[4]
	KB = FP/Q[3]

	D = Sqrt((KP+KE)^2-4*KE*KB)
	Kpos = 0.5*(KP+KE+D)
	Kneg = 0.5*(KP+KE-D)
	Eneg = (Kpos-KB)/(Kpos-Kneg)

	Q = [Q[0],Q[1],Kpos,Kneg,Eneg,Q[5]]

	;P = [FA,FV,Kpos,Kneg,Eneg,FI]

	ni=X[0] & n=n_elements(X[ni+1:*])/3
	ti=X[1:ni] & time=X[ni+1:ni+n]
	inputA=X[ni+n+1:ni+2*n] & inputV=X[ni+2*n+1:*]

	EApos = ExpConvolution(Q[2],[time,inputA],Der=DApos)
	EAneg = ExpConvolution(Q[3],[time,inputA],Der=DAneg)
	EVpos = ExpConvolution(Q[2],[time,inputV],Der=DVpos)
	EVneg = ExpConvolution(Q[3],[time,inputV],Der=DVneg)

	CE = (Q[0]*EAneg+Q[1]*EVneg-Q[0]*EApos-Q[1]*EVpos)/(1/Q[3]-1/Q[2])/(Q[0]+Q[1])
	IE 	= IntVector(time,CE)

	EApos=EApos[ti] & EAneg=EAneg[ti]
	DApos=DApos[ti] & DAneg=DAneg[ti]
	EVpos=EVpos[ti] & EVneg=EVneg[ti]
	DVpos=DVpos[ti] & DVneg=DVneg[ti]
	IE=IE[ti]

	F = Q[0]*(1-Q[4])*EApos + Q[0]*Q[4]*EAneg + Q[1]*(1-Q[4])*EVpos + Q[1]*Q[4]*EVneg + Q[5]*IE

	IF n_params() LT 4 THEN RETURN

;	F0 = (1-P[4])*EApos + P[4]*EAneg + P[5]*(IntVector(X[0:n-1],EAneg-EApos)/(1/P[3]-1/P[2]) - IE)/(P[0]+P[1])
;	F1 = (1-P[4])*EVpos + P[4]*EVneg + P[5]*(IntVector(X[0:n-1],EVneg-EVpos)/(1/P[3]-1/P[2]) - IE)/(P[0]+P[1])
;	F2 = P[0]*(1-P[4])*DApos + P[1]*(1-P[4])*DVpos 	- P[5]*(IntVector(X[0:n-1],P[0]*DApos+P[1]*DVpos)/(P[0]+P[1]) - IE/(P[2]^2))/(1/P[3]-1/P[2])
;	F3 = P[0]*P[4]*DAneg + P[1]*P[4]*DVneg 			- P[5]*(IntVector(X[0:n-1],P[0]*DAneg+P[1]*DVneg)/(P[0]+P[1]) + IE/(P[3]^2))/(1/P[3]-1/P[2])
;	F4 = -P[0]*EApos + P[0]*EAneg - P[1]*EVpos + P[1]*EVneg
;	F5 = IE
;
;	Fi = [[F0],[F1],[F2],[F3],[F4],[F5]]
end