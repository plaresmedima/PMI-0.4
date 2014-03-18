Pro DualInletUptakeModel, X, P, F, Fi

	if n_params() eq 0 then return

	;P = [FA,FV,KT,E]

	n = n_elements(X)/3

	X0 = X[0:2*n-1]
	X1 = [X[0:n-1],X[2*n:3*n-1]]

	I0 	= IntVector(X0[0:n-1],X0[n:2*n-1])
	I1 	= IntVector(X1[0:n-1],X1[n:2*n-1])
	X0 	= ExpConvolution(P[2],X0,Der=D0)
	X1 	= ExpConvolution(P[2],X1,Der=D1)

	F = P[0]*(1-P[3])*X0 + P[1]*(1-P[3])*X1 + P[0]*P[3]*I0 + P[1]*P[3]*I1

	F0 = (1-P[3])*X0 + P[3]*I0
	F1 = (1-P[3])*X1 + P[3]*I1
	F2 = P[0]*(1-P[3])*D0 + P[1]*(1-P[3])*D1
	F3 = -P[0]*X0 - P[1]*X1 + P[0]*I0 + P[1]*I1

	Fi = [[F0],[F1],[F2],[F3]]
end

function FitDualInletUptake, time, curve, aif, vif, Pars=P, AIC=AIC, constrained=constrained, _EXTRA=e

	if n_params() eq 0 then return, 0

	;P = [FA,FV,PT,E]

	P[2] = 1/P[2]

  	if keyword_set(constrained) then begin
  		parinfo = replicate({limited:[0,0], limits:[0.D,0]}, 4)
  		parinfo[*].limited[0]=1
;  		info[3].limited[1]=1
;  		info[3].limits[1]=1
  	endif
    Fit = mpcurvefit([time,aif,vif],curve,1+curve*0,P,FUNCTION_name='DualInletUptakeModel',parinfo=parinfo, _EXTRA=e)

	P[2] = 1/P[2]

	if arg_present(AIC) then begin
		n = n_elements(Curve)
		AIC = n*alog(total((Curve-Fit)^2)/n) + 2D*(1+n_elements(P))
	endif

	return, Fit
end