FUNCTION QIM_DCE_1CMD_FITCONC, Ct, X, delay

    IF norm(ct) EQ 0. THEN RETURN, ct

	n = (n_elements(X)-1)/2
	t = X[0:n-1]
 	ca = ShiftAif(X[n:2*n-1],t,Delay)

    dt = (t[1:n-1]-t[0:n-2])/2E
    ca1 = [0, TOTAL( dt*(ca[0:n-2]+ca[1:n-1]), /cumulative)]
    ct1 = [0, TOTAL( dt*(ct[0:n-2]+ct[1:n-1]), /cumulative)]

	A = TRANSPOSE([[ct1],[ca1]])
	SVDC, A, W, U, V
	Y = TRANSPOSE(U) ## TRANSPOSE([ct])
	kpos = where(W GT 0, npos, COMPLEMENT=kzero, NCOMPLEMENT=nzero)
	if npos GT 0 then Y[kpos] /= W[kpos]
	if nzero GT 0 then Y[kzero] = 0

	RETURN, A ## (V ## Y)

END

FUNCTION QIM_DCE_1CMD_FIT, S, X

	dDelay = 0.25 ;sec
	maxDelay = 15.0 ;sec
	nDelay = 1+ceil(maxDelay/dDelay)
	Delay = dDelay*findgen(nDelay)

    n0 = X[n_elements(X)-1]
    S0 = total(S[0:n0-1])/n0
    C = S - S0

    Error = fltarr(nDelay)
    for i=0L, nDelay-1 do begin
    	Cfit = QIM_DCE_1CMD_FITCONC(C, X, Delay[i])
    	Error[i] = total((Cfit-C)^2)
    endfor
    tmp = min(Error,i)
	Cfit = QIM_DCE_1CMD_FITCONC(C, X, Delay[i])

	RETURN, S0 + Cfit

END


FUNCTION QIM_DCE_1CMD_PRECOMPUTE, X

   RETURN, X
END


PRO QIM_DCE_1CMD
END