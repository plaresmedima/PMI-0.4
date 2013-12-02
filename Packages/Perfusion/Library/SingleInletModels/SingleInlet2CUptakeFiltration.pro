;C(t) = (1-E) FP exp(-t/TP)*Ca(t) + E FP * Ca(t)
;P = [VP, FP, FE/FP]
;TP = VP/FP
;E = FE/FP

Pro SingleInlet2CUptakeFiltration, X, P, C, C_DER

	if n_params() eq 0 then return

	ni=X[0] & n=n_elements(X[ni+1:*])/2
	ti=X[1:ni] & time=X[ni+1:ni+n] & input=X[ni+n+1:*]

	K = P[1]/P[0] ; FP/VP

	conv = ExpConvolution(K,[time,input],Der=dconv)
	Integral = IntVector(time,input)

	C = (1-P[2])*P[1]*conv[ti] + P[2]*P[1]*Integral[ti]

	IF n_params() LT 4 THEN return

	;Derivatives wrt model parameters

	K_DER0 = -P[1]/P[0]^2
	K_DER1 = 1/P[0]
	K_DER2 = 0

	C_DER0 = (1-P[2])*P[1]*dconv[ti]*K_DER0
	C_DER1 = (1-P[2])*conv[ti] + (1-P[2])*P[1]*dconv[ti]*K_DER1 + P[2]*Integral[ti]
	C_DER2 = -P[1]*conv[ti] + (1-P[2])*P[1]*dconv[ti]*K_DER2 + P[1]*Integral[ti]

	C_DER = [[C_DER0],[C_DER1],[C_DER2]]
end