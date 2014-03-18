;C = CA *  V (exp(-t/T)-exp(-t/TA))/(T-TA)
;P = [V,T,TA]

Pro  SingleInletCompartmentDispersion, X, P, C, C_DER

	if n_params() eq 0 then return

	ni=X[0] & n=n_elements(X[ni+1:*])/2
	ti=X[1:ni] & time=X[ni+1:ni+n] & input=X[ni+n+1:*]

	ConvT = ExpConvolution(1/P[1],[time,input],Der=dConvT)
	ConvA = ExpConvolution(1/P[2],[time,input],Der=dConvA)

	C = P[0]*(ConvT[ti]-ConvA[ti])/(P[1]-P[2])

	IF n_params() LT 4 THEN return

	;Derivatives wrt model parameters

	C_DER0 = C/P[0]
	C_DER1 = -P[0]*(dConvT[ti]/P[1]^2)/(P[1]-P[2]) - C/(P[1]-P[2])
	C_DER2 = +P[0]*(dConvA[ti]/P[2]^2)/(P[1]-P[2]) + C/(P[1]-P[2])

	C_DER = [[C_DER0],[C_DER1],[C_DER2]]
end