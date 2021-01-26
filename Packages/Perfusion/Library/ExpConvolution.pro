;
;
;    Copyright (C) 2009 Steven Sourbron

function ExpConvolution, l, X, Der=DY

	n = n_elements(X)/2
	Y = dblarr(n)

	T = X[0:n-1]
	A = X[n:*]

	if finite(l) eq 0 then begin
		if arg_present(DY) then DY=Y
		return, IntVector(T,A) ;corrected 14/01/2021
	endif

	DT = T[1:n-1]-T[0:n-2]
	DA = A[1:n-1]-A[0:n-2]

	Z = l*DT

	E = exp(-Z)
	E0 = 1-E
	E1 = Z-E0

	Il = (A[0:n-2]*E0 + DA*E1/Z)/l

	for i=0L,n-2 do Y[i+1] = E[i]*Y[i] + Il[i]

	if not arg_present(DY) then return, Y

	E2 = Z^2-2*E1

    DIl = -DT*Il + (A[0:n-2]*E1 + DA*E2/Z )/(l^2)
    DIl = -E*DT*Y + DIl

	DY = dblarr(n)

	for i=0L,n-2 do DY[i+1] = E[i]*DY[i] + DIl[i]

	return, Y
end
