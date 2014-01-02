;P[0] = Arterial Inflow
;P[1] = Venous Inflow
;P[2] = Extracelular Volume

Pro DualInletCompartment, X, P, F, Fi

	if n_params() eq 0 then return

	;P = [FA,FV,EV]

	KT = (P[0]+P[1])/P[2]

	ni=X[0] & n=n_elements(X[ni+1:*])/3
	ti=X[1:ni] & time=X[ni+1:ni+n]
	inputA=X[ni+n+1:ni+2*n] & inputV=X[ni+2*n+1:*]

	F0 = ExpConvolution(KT,[time,inputA],Der=D0)
	F1 = ExpConvolution(KT,[time,inputV],Der=D1)

	F0=F0[ti] & F1=F1[ti]
	D0=D0[ti] & D1=D1[ti]

	F = P[0]*F0 + P[1]*F1

	IF n_params() LT 4 THEN RETURN

;	F2 	= P[0]*D0 + P[1]*D1
;	Fi = [[F0],[F1],[F2]]
end