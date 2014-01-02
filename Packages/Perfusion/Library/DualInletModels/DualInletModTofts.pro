;Ci(t) = AF Ca(t) + (1-AF) Cv(t)
;C(t) = VE Ci(t) + KI exp(-t/TI)*Ci(t)

;P = [AF, VE+VI, VI/(VE+VI), KI]

Pro DualInletModTofts, X, P, F, Fi

	IF n_params() EQ 0 THEN RETURN

	ni=X[0] & n=n_elements(X[ni+1:*])/3
	ti=X[1:ni] & time=X[ni+1:ni+n]
	inputA=X[ni+n+1:ni+2*n] & inputV=X[ni+2*n+1:*]

	VI = P[1]*P[2]
	VE = P[1]*(1-P[2])
	TIC = VI/P[3]

	input = P[0]*inputA + (1-P[0])*inputV
	Conv = ExpConvolution(1/TIC,[time,input])

	F = VE*input[ti] + P[3]*Conv[ti]

	IF n_params() LT 4 THEN RETURN

end