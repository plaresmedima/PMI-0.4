FUNCTION VFA_Linear_T1fit, TR, FA, S, FIT=YFIT

	Y = S/sin(!PI*FA/180)
	X = S/tan(!PI*FA/180)

	VEC = LINFIT(X,Y)

	A = VEC[0]
	B = VEC[1]

	if arg_present(YFIT) then YFIT = A + B*X

	R1 = -alog(B)/TR
	S0 = A/(1-B)

	RETURN, [R1,S0]

END