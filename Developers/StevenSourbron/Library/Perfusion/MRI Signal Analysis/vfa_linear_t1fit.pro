FUNCTION VFA_Linear_T1fit, TR, FA, S, RMS=RMS, PROB=prob

	Y = S/sin(!PI*FA/180)
	X = S/tan(!PI*FA/180)

	VEC = LINFIT(X,Y, PROB=prob)

	A = VEC[0]
	B = VEC[1]

	if arg_present(RMS) then begin
		YFIT = A + B*X
		nFA = n_elements(FA)
		RMS = sqrt(total((Y-YFIT)^2)/nFA)
		RMS = 100 * RMS / sqrt(total(Y^2)/nFA)
	endif

	R1 = -alog(B)/TR
	S0 = A/(1-B)

	RETURN, [R1,S0]

END