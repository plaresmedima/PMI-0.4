;C(t) = Sinf*(1-exp(-Td*R1))

;P[0] = Sinf
;P[1] = R1

Pro PMI__FitT1SaturationRecovery, X, P, C, C_DER

	if n_params() eq 0 then return

	C_DER0 = 1-exp(-X*P[1])

	C = P[0]*C_DER0

	IF n_params() LT 4 THEN return

	C_DER1 = P[0]*X*exp(-X*P[1])

	C_DER = [[C_DER0],[C_DER1]]
end

