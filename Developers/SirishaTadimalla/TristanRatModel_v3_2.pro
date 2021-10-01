;  vh ch' = khe ce - kbh ch
;  ch = exp(-t kbh/vh) * (khe/vh) ce

;	DR1(t) = re ve (1-E) cp(t) + rh khe exp(-t Kc) * (1-E) cp(t)
;	P = [khe, kbh]

; 	Kc = kbh/vh
;	E = khe /(Fp + khe)

Pro TristanRatModel_v3_2, X, P, DR1

	if n_params() eq 0 then return

	re = X[0]
	rh = X[1]
	ve = X[2]
	vh = X[3]
	fp = X[4]
	ni = X[5]
	ti = X[6:6+ni-1]
	n = n_elements(X[6+ni:*])/2
	time = X[6+ni:6+ni+n-1]
	cp = X[6+ni+n:*]
	khe = P[0]
	kbh = P[1]

	Kc = kbh/vh
	E = khe /(Fp + khe)
	ce = (1-E)*cp

	Conv = ExpConvolution(Kc,[time,ce])

	DR1 = re * ve * ce[ti] + rh * khe * Conv[ti]

end