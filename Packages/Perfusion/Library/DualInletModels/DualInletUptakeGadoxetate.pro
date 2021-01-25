


Pro DualInletUptakeGadoxetate, X, P, F, Fi

	IF n_params() EQ 0 THEN RETURN

	;P = [FA,FV,TE,Eapp]

	ni=X[0] & n=n_elements(X[ni+1:*])/3
	ti=X[1:ni] & time=X[ni+1:ni+n]
	cA=X[ni+n+1:ni+2*n] & cV=X[ni+2*n+1:*]

	Jin = P[0]*cA + P[1]*cV

	A = ExpConvolution(1/P[2],[time,Jin])
	B = IntVector(time,A)*(P[3]/P[2])

	F = A + B

end