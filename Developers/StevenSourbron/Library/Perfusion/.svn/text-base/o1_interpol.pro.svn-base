function O1_interpol, Xin,Yin,Xi

	X=Xin & Y=Yin

	n 	= n_elements(X)

	if X[0] gt X[n-1] then begin
		X = ReverseArray(X)
		Y = ReverseArray(Y)
	endif

	ni 	= n_elements(Xi)
	Yi 	= Xi
	Ind = lindgen(n)

	for i=0L,ni-1 do begin

		outside = (Xi[i] lt X[0]) or (Xi[i] ge X[n-1])

		if outside then begin
			if Xi[i] lt X[0] then begin
				I0 = 0
				I1 = 1
			endif
			if Xi[i] ge X[n-1] then begin
				I0 = n-2
				I1 = n-1
			endif
		endif else begin
			cnt = total(X le Xi[i])
			I0 = Ind[cnt-1]
			I1 = Ind[cnt]
		endelse

		X0 = X[I0] & Y0 = Y[I0]
		X1 = X[I1] & Y1 = Y[I1]
		slope = (Y1-Y0)/(X1-X0)
		DX = Xi[i]-X0
		Yi[i] = Y0 + slope * DX
	endfor

	return, Yi

end