;X = [t,CA]
;A = [TT,VT]
;F = Ctt


PRO RENALMODEL_INVERSE_NONLINEAR_FUNC_2_, X, A, Ctt, Cder

	N = N_elements(X)/2
	t = X[0:N-1]
	CA = X[N:2*N-1]

	TP = A[0]
	TT = A[1]
	VP = A[2]
	VT = A[3]

    EP = Expconvolution(1/Tp,[t,CA],Der=dEP_KP)
    Cp = EP/Tp

    ET = Expconvolution(1/TT,[t,Cp],Der=dET_KT)
    CT = ET/TT

	Ctt = VP*Cp + VT*CT

	if n_params() eq 3 then return

    dCT_TT = (dET_KT/TT + ET) * (-1/TT^2)
    dCP_TP = (dEP_KP/TP + EP) * (-1/TP^2)
    dCT_TP = Expconvolution(1/TT,[t,dCP_TP]) /TT

    TPder = VP * dCP_TP + VT * dCT_TP
    TTder = VT * dCT_TT

    VPder = Cp
    VTder = Ct

    Cder = [[TPder], [TTder],[VPder],[VTder]]

END