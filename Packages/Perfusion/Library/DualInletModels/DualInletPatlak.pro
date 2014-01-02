;Ci(t) = AF Ca(t) + (1-AF) Cv(t)
;C(t) = EV Ci(t) + Ki * Ci(t)

;P = [AF,EV,KI] ;[Arterial Flow Fraction, Extracellular Volume, Intracellular Uptake Rate]

Pro DualInletPatlak, X, P, F, Fi

	IF n_params() EQ 0 THEN RETURN

	ni=X[0] & n=n_elements(X[ni+1:*])/3
	ti=X[1:ni] & time=X[ni+1:ni+n]
	inputA=X[ni+n+1:ni+2*n] & inputV=X[ni+2*n+1:*]

	Input = P[0]*inputA + (1-P[0])*inputV
	Integral = IntVector(time,input)

	F = P[1]*Input[ti] + P[2]*Integral[ti]

	IF n_params() LT 4 THEN RETURN

end