;P[0] = Arterial Inflow
;P[1] = Venous Inflow
;P[2] = 1/Mean Transit Time

Pro FitDualInletCompartmentModel, X, P, F, Fi

	n = n_elements(X)/3

	X0 = X[0:2*n-1]
	X1 = [X[0:n-1],X[2*n:3*n-1]]

	F0 = ExpConvolution(P[2],X0,Der=D0)
	F1 = ExpConvolution(P[2],X1,Der=D1)

	F 	= P[0]*F0 + P[1]*F1
	F2 	= P[0]*D0 + P[1]*D1

	Fi = [[F0],[F1],[F2]]
end

function FitDualInletCompartment, time, curve, aif, vif, Pars=P, AIC=AIC, constrained=constrained, _EXTRA=e

	if n_params() eq 0 then return, 0

	;P = [FA,FV,PT]

	P = [P[0],P[1],1/P[2]]

  	if keyword_set(constrained) then begin
  		parinfo = replicate({limited:[0,0], limits:[0.D,0]}, 3)
  		parinfo[*].limited[0]=1
  	endif
    fit = mpcurvefit([time,aif,vif],curve,1+curve*0,P,FUNCTION_name='FitDualInletCompartmentModel',parinfo=parinfo,_EXTRA=e)

	P = [P[0],P[1],1/P[2]]

	if arg_present(AIC) then begin
		n = n_elements(Curve)
		AIC = n*alog(total((Curve-Fit)^2)/n) + 2D*(1+n_elements(P))
	endif

	return, Fit
end