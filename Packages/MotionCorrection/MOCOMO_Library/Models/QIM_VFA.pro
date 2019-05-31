PRO QIM_VFA_SPGR_SS, X, P, S

	if n_params() eq 0 then return

	E = exp(-X[0]*P[1])

	FA = !PI*X[1:*]/180.

	S = sin(FA) * P[0] * (1-E)/(1-cos(FA)*E)

END

FUNCTION QIM_VFA_FIT, S, X, P

;X = [TR, FA1, FA2, ...]
;TR (ms), FA (deg)

	nP = 2
	parinfo = replicate({limited:bytarr(2), limits:dindgen(2)}, nP)
	parinfo[*].limited[0] = 1

	P = [2*max(S), 1/1000.0]

	return, mpcurvefit(X, S, 1+0E*S, P, function_name='QIM_VFA_SPGR_SS',/quiet,PARINFO=parinfo,/NODERIVATIVE)


END


FUNCTION QIM_VFA_PRECOMPUTE, X

END


PRO QIM_VFA
END