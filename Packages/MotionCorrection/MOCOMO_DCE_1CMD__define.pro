FUNCTION MOCOMO_DCE_1CMD::FIT_CONC, Ct, delay

    IF norm(ct) EQ 0. THEN RETURN, ct

    X = *self.Independent
	n = (n_elements(X)-1)/2
	t = X[0:n-1]
 	ca = ShiftAif(X[n:2*n-1],t,Delay)

    dt = (t[1:n-1]-t[0:n-2])/2E
    ca1 = [0, TOTAL( dt*(ca[0:n-2]+ca[1:n-1]), /cumulative)]
    ct1 = [0, TOTAL( dt*(ct[0:n-2]+ct[1:n-1]), /cumulative)]

	A = TRANSPOSE([[ct1],[ca1]])
	SVDC, A, W, U, V
	X = TRANSPOSE(U) ## TRANSPOSE([ct])
	kpos = where(W GT 0, npos, COMPLEMENT=kzero, NCOMPLEMENT=nzero)
	if npos GT 0 then X[kpos] /= W[kpos]
	if nzero GT 0 then X[kzero] = 0

	RETURN, A ## (V ## X)

END

FUNCTION MOCOMO_DCE_1CMD::FIT_SIGNAL, S

	dDelay = 0.25 ;sec
	maxDelay = 15.0 ;sec
	nDelay = 1+ceil(maxDelay/dDelay)
	Delay = dDelay*findgen(nDelay)

	X = *self.Independent
    n0 = X[n_elements(X)-1]
    S0 = total(S[0:n0-1])/n0
    C = S - S0

    Error = fltarr(nDelay)
    for i=0L, nDelay-1 do begin
    	Cfit = self->FIT_CONC(C, Delay[i])
    	Error[i] = total((Cfit-C)^2)
    endfor
    tmp = min(Error,i)
	Cfit = self->FIT_CONC(C, Delay[i])

	RETURN, S0 + Cfit

END


PRO MOCOMO_DCE_1CMD::SET_MODEL, X

   self.independent = ptr_new(X)

END


PRO MOCOMO_DCE_1CMD__DEFINE

  struct = {MOCOMO_DCE_1CMD, Dummy:0B}

END