Pro NonlinearPatlakModel, X, P, F, Fi

	n = n_elements(X)/2

	F0 	= X[n:2*n-1]
	F1 	= IntVector(X[0:n-1],X[n:2*n-1])
	F 	= P[0]*F0 + P[1]*F1

	Fi = [[F0],[F1]]
end


function FitPatlakNonlinear, time, Curve, aif, Pars=P, AIC=AIC,Init=i, mp=mp, quiet=quiet $
	, noderivative = noderivative, constrained = constrained

	if n_elements(i) ne 0 then begin

		Pc = TwoCompartmentInitialValues(i,'C')
		P = [Pc[2],Pc[0]] ;[V1,K21*V1] or [V1,F21]
	endif

	if keyword_set(mp) then begin
		parinfo = replicate({limited:[0,0], limits:[0.D,0]}, 2)
  		if keyword_set(constrained) then parinfo[*].limited[0]=1
		fit = mpcurvefit([time,aif], curve, curve*0+1, P, function_name='NonlinearPatlakModel' $
			, status=status, noderivative=noderivative, quiet=quiet,parinfo=parinfo)
	endif else begin
		fit = curveFit([time,aif],Curve,1+Curve*0,P,/double,function_name='NonlinearPatlakModel')
	endelse

	if arg_present(AIC) then begin
		n = n_elements(Curve)
		AIC = n*alog(total((Curve-Fit)^2)/n) + 2D*(1+n_elements(P))
	endif

	return, fit
end

function FitPatlakNonlinearDelay, time, roicurve, aif,Delay=Delay, Pars=P,AIC=AIC,Init=i,mp=mp,noderivative=noderivative,quiet=quiet,constrained=constrained,nodelay=nodelay

	if n_elements(i) ne 0 then begin

		Pc = TwoCompartmentInitialValues(i,'C')
		P = [Pc[2],Pc[0]] ;[V1,K21*V1] or [V1,F21]
	endif

	if keyword_set(nodelay) then begin
		Fit=FitPatlakNonlinear(Time,RoiCurve,Aif,Pars=P,AIC=AIC,mp=mp,noderivative=noderivative,quiet=quiet,constrained=constrained)
		Delay = 0.0
		return, Fit
	endif

	if n_elements(Delay) eq 0 then Delay = findgen(21)

	nDel = n_elements(Delay)
	Err = dindgen(nDel)

	for i=0L,nDel-1 do begin

		Pi = P
		Fit = FitPatlakNonlinear(Time,RoiCurve,ShiftAif(Aif,Time,Delay[i]),Pars=Pi,mp=mp,noderivative=noderivative,quiet=quiet,constrained=constrained)
		Err[i] = total((RoiCurve-Fit)^2)
	endfor

	tmp = min(Err,i)
	Delay = Delay[i]
	Fit = FitPatlakNonlinear(Time,RoiCurve,ShiftAif(Aif,Time,Delay),Pars=P,AIC=AIC,mp=mp,noderivative=noderivative,quiet=quiet,constrained=constrained)

	AIC = AIC + 2D

	return, Fit
end