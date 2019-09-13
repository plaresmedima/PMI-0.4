
;	C(t) = ve Ce(t) + khe exp(-t/Tc) * Ce(t)
;	P = [khe, kbh]

; 	Tc = (1-ve)/kbh

Pro SingleInletTristanRatModel, X, P, C, C_DER

	if n_params() eq 0 then return

	ni=X[0] & n=n_elements(X[ni+1:*])/2
	ti=X[1:ni] & time=X[ni+1:ni+n] & input=X[ni+n+1:*]

	ve = 0.23
	Kc = P[1]/(1-ve) ; kbh / (1-ve) = 1/Tc

	Conv = ExpConvolution(Kc,[time,input])

	C = ve*input[ti] + P[0]*Conv[ti]

end