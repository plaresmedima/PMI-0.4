Pro DualInletUptake, X, P, F, Fi

	IF n_params() EQ 0 THEN RETURN

	;P = [FA,FV,EV,E]

	KT = (P[0]+P[1])/P[2]

	ni=X[0] & n=n_elements(X[ni+1:*])/3
	ti=X[1:ni] & time=X[ni+1:ni+n]
	inputA=X[ni+n+1:ni+2*n] & inputV=X[ni+2*n+1:*]

	I0 	= IntVector(time,inputA)
	I1 	= IntVector(time,inputV)
	X0 	= ExpConvolution(KT,[time,inputA],Der=D0)
	X1 	= ExpConvolution(KT,[time,inputV],Der=D1)

	I0=I0[ti] & I1=I1[ti]
	X0=X0[ti] & X1=X1[ti]
	D0=D0[ti] & D1=D1[ti]

	F = P[0]*(1-P[3])*X0 + P[1]*(1-P[3])*X1 + P[0]*P[3]*I0 + P[1]*P[3]*I1

	IF n_params() LT 4 THEN RETURN

;	F0 = (1-P[3])*X0 + P[3]*I0
;	F1 = (1-P[3])*X1 + P[3]*I1
;	F2 = P[0]*(1-P[3])*D0 + P[1]*(1-P[3])*D1
;	F3 = -P[0]*X0 - P[1]*X1 + P[0]*I0 + P[1]*I1
;
;	Fi = [[F0],[F1],[F2],[F3]]
end