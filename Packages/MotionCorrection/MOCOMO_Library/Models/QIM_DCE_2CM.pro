FUNCTION QIM_DCE_2CM_FIT, S, X

    n0 = X[0]
    S0 = total(S[0:n0-1])/n0
    Ct = S - S0

    IF norm(ct) EQ 0. THEN RETURN, S0+ct

    n = n_elements(ct)
    dt = X[1:n-1]
    ca1 = X[n:2*n-1]
    ca2 = X[2*n:3*n-1]

    ct1 = [0, TOTAL( dt*(ct[0:n-2]+ct[1:n-1]) , /cumulative)]
    ct2 = [0, TOTAL( dt*(ct1[0:n-2]+ct1[1:n-1]) , /cumulative)]

	A = TRANSPOSE([[-ct2],[-ct1],[ca1],[ca2]])
	SVDC, A, W, U, V
	Y = TRANSPOSE(U) ## TRANSPOSE([ct])
	kpos = where(W GT 0, npos, COMPLEMENT=kzero, NCOMPLEMENT=nzero)
	if npos GT 0 then Y[kpos] /= W[kpos]
	if nzero GT 0 then Y[kzero] = 0

	Cfit = A ## (V ## Y)

	RETURN, S0 + Cfit

END

FUNCTION QIM_DCE_2CM_PRECOMPUTE, X

	n = (n_elements(X)-1)/2

	t = X[0:n-1]
	ca = X[n:2*n-1]
	n0 = X[2*n]

    dt = (t[1:n-1]-t[0:n-2])/2E
    ca1 = [0, TOTAL( dt*(ca[0:n-2]+ca[1:n-1]) , /cumulative)]
    ca2 = [0, TOTAL( dt*(ca1[0:n-2]+ca1[1:n-1]) , /cumulative)]

    return, [n0,dt,ca1,ca2]

END

PRO QIM_DCE_2CM
END