


Pro DualInletTristan, X, P, F, Fi

	IF n_params() EQ 0 THEN RETURN

	;P = [FA,FV,TE,TH,K]

	ni=X[0] & n=n_elements(X[ni+1:*])/3
	ti=X[1:ni] & time=X[ni+1:ni+n]
	DR1a = X[ni+n+1:ni+2*n]
	DR1v = X[ni+2*n+1:*]

	Jin = P[0]*DR1a + P[1]*DR1v

	A = ExpConvolution(1/P[2],[time,Jin])
	B = ExpConvolution(1/P[3],[time,A])

	F = A + P[4]*B

end