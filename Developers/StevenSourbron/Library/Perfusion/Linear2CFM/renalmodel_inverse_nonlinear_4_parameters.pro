
PRO RENALMODEL_INVERSE_NONLINEAR_4_PARAMETERS, Ctt, CA, t, Tprec,TTrec, VPrec, VTrec

	Tpinit = 3.0D
	TTinit = 60.0D
	VPinit = 0.1D
	VTinit = 0.3D
	X = [t,CA]
	A = [Tpinit,TTinit, VPinit, VTinit]
	Weights = 1+0*t
;	yfit = CURVEFIT(X, Ctt, weights, A, FUNCTION_NAME='RENALMODEL_INVERSE_NONLINEAR_FUNC_2_', NODERIVATIVE=1, STATUS=QUIET)
	yfit = MPCURVEFIT(X, Ctt, weights, A, FUNCTION_NAME='RENALMODEL_INVERSE_NONLINEAR_FUNC_2_', NODERIVATIVE=0, /QUIET)
    Tprec = A[0]
    TTrec = A[1]
    VPrec = A[2]
	VTrec = A[3]

END