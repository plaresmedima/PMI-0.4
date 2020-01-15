;C(t) = Sinf*(1-Sratio*exp(-Td*R1))

;P[0] = Sinf
;P[1] = Sratio
;P[2] = R1

Pro PMI__TRISTAN_MOLLI_T1mapping, X, P, C, C_DER

	if n_params() eq 0 then return

	E = exp(-X*P[2])

	C_DER0 = 1-P[1]*E

	C = ABS(P[0]*C_DER0)

	IF n_params() LT 4 THEN return

	C_DER1 = -P[0]*E
	C_DER2 = P[0]*P[1]*X*E

	C_DER = [[C_DER0],[C_DER1],[C_DER2]]
end

