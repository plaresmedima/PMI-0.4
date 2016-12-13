function O1_interpol_2D, X,Y,Xi

	n  = n_elements(X)
	ni = n_elements(Xi)

	Yi 	= make_array(n_elements(Y[*,0]),ni,value=Y[0])
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

		X0 = X[I0] & Y0 = reform(Y[*,I0],/overwrite)
		X1 = X[I1] & Y1 = reform(Y[*,I1],/overwrite)

		A = (Xi[i]-X0)/(X1-X0)

		Yi[*,i] = Y0*(1-A) + Y1*A
	endfor

	return, Yi

end