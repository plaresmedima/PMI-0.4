;C(t) = F exp(-t/T)*Ca(t)
;P = [VP+VE, F]

;T =(VP+VE)/F

;

Pro SingleInletCompartment, X, P, C, C_DER

	if n_params() eq 0 then return

	ni=X[0] & n=n_elements(X[ni+1:*])/2
	ti=X[1:ni] & time=X[ni+1:ni+n] & input=X[ni+n+1:*]

	K = P[1]/P[0] ; F/V

	Conv = ExpConvolution(K,[time,input],Der=dConv)

	C = P[1]*Conv[ti]

	IF n_params() LT 4 THEN return

	;Derivatives wrt model parameters

	K_DER0 = -P[1]/P[0]^2
	K_DER1 = 1/P[0]

	C_DER0 = P[1]*dConv[ti]*K_DER0
	C_DER1 = Conv[ti] + P[1]*dConv[ti]*K_DER1

	C_DER = [[C_DER0],[C_DER1]]
end