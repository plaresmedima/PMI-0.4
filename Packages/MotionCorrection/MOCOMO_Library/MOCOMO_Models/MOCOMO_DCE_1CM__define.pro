FUNCTION MOCOMO_DCE_1CM::FIT_SIGNAL, S

    X = *self.independent
    n0 = X[0]
    S0 = total(S[0:n0-1])/n0
    Ct = S - S0

    IF norm(ct) EQ 0. THEN RETURN, S0+ct

    n = n_elements(ct)
    dt = X[1:n-1]
    ca1 = X[n:2*n-1]

    ct1 = [0, TOTAL( dt*(ct[0:n-2]+ct[1:n-1]) , /cumulative)]

	A = TRANSPOSE([[ct1],[ca1]])
	SVDC, A, W, U, V
	X = TRANSPOSE(U) ## TRANSPOSE([ct])
	kpos = where(W GT 0, npos, COMPLEMENT=kzero, NCOMPLEMENT=nzero)
	if npos GT 0 then X[kpos] /= W[kpos]
	if nzero GT 0 then X[kzero] = 0

	Cfit = A ## (V ## X)

	RETURN, S0 + Cfit

END


PRO MOCOMO_DCE_1CM::SET_MODEL, X

	n = (n_elements(X)-1)/2

	t = X[0:n-1]
	ca = X[n:2*n-1]
	n0 = X[2*n]

    dt = (t[1:n-1]-t[0:n-2])/2E
    ca1 = [0, TOTAL( dt*(ca[0:n-2]+ca[1:n-1]) , /cumulative)]

    self.independent = ptr_new([n0,dt,ca1])

END


PRO MOCOMO_DCE_1CM__DEFINE

  struct = {MOCOMO_DCE_1CM, Dummy:0B}

END