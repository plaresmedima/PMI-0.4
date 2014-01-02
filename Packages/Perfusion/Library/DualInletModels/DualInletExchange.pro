Pro DualInletExchange, X, P, F, Fi

	IF n_params() EQ 0 THEN RETURN

	;P = [FA,FV,VE+VP,E,VP/(VE+VP)]

	KI = (P[0]+P[1])*P[3]/(1-P[3])
	VP = P[2]*P[4]
	VE = P[2]-VP
	Q = [P[0],P[1],KI,VP,VE]

	;P = [FA,FV,KI,VP,VE]

	FP = Q[0]+Q[1]
	KP = (FP+Q[2])/Q[3]
	KE = Q[2]/Q[4]
	KB = FP/Q[3]

	D = Sqrt((KP+KE)^2-4*KE*KB)
	Kpos = 0.5*(KP+KE+D)
	Kneg = 0.5*(KP+KE-D)
	Eneg = (Kpos-KB)/(Kpos-Kneg)

	Q = [Q[0],Q[1],Kpos,Kneg,Eneg]

	;P = [FA,FV,Kpos,Kneg,Eneg]

	ni=X[0] & n=n_elements(X[ni+1:*])/3
	ti=X[1:ni] & time=X[ni+1:ni+n]
	inputA=X[ni+n+1:ni+2*n] & inputV=X[ni+2*n+1:*]

	EApos = ExpConvolution(Q[2],[time,inputA],Der=DApos)
	EAneg = ExpConvolution(Q[3],[time,inputA],Der=DAneg)
	EVpos = ExpConvolution(Q[2],[time,inputV],Der=DVpos)
	EVneg = ExpConvolution(Q[3],[time,inputV],Der=DVneg)

	EApos=EApos[ti] & EAneg=EAneg[ti]
	DApos=DApos[ti] & DAneg=DAneg[ti]
	EVpos=EVpos[ti] & EVneg=EVneg[ti]
	DVpos=DVpos[ti] & DVneg=DVneg[ti]

	F = Q[0]*(1-Q[4])*EApos + Q[0]*Q[4]*EAneg + Q[1]*(1-Q[4])*EVpos + Q[1]*Q[4]*EVneg

	IF n_params() LT 4 THEN RETURN

;	F0 = (1-P[4])*EApos + P[4]*EAneg
;	F1 = (1-P[4])*EVpos + P[4]*EVneg
;	F2 = P[0]*(1-P[4])*DApos + P[1]*(1-P[4])*DVpos
;	F3 = P[0]*P[4]*DAneg + P[1]*P[4]*DVneg
;	F4 = -P[0]*EApos + P[0]*EAneg - P[1]*EVpos + P[1]*EVneg
;
;	Fi = [[F0],[F1],[F2],[F3],[F4]]
end