Pro ToftsModel, X, P, F, Fi

	n = n_elements(X)/2

	F0 	= X[n:2*n-1]
	F1 	= ExpConvolution(P[2],X,Der=D1)
	F 	= P[0]*F0 + P[1]*F1
	F2 	= P[1]*D1

	Fi = [[F0],[F1],[F2]]
end

function ToftsFitResidue, time, curve, aif, pars=p, status=status $
	,	mp=mp, noderivative = noderivative, quiet=quiet, constrained=constrained
	if keyword_set(mp) then begin
		parinfo = replicate({limited:[0,0], limits:[0.D,0]}, 3)
	  if keyword_set(constrained) then parinfo[*].limited[0]=1
		fit = mpcurvefit([time,aif], curve, curve*0+1, P, function_name='ToftsModel' $
			, status=status, noderivative=noderivative, quiet=quiet, parinfo=parinfo)
	endif else begin
		fit = curveFit([time,aif],Curve,1+Curve*0,P,/double,function_name='ToftsModel')
	endelse
	return, fit
end


function FitTofts, time, Curve, aif, Pars=P,Init=i,Convective=Pc, mp=mp, quiet=quiet $
	, noderivative = noderivative, aic = aic, constrained=constrained


	if n_elements(i) ne 0 then begin

		Pc = TwoCompartmentInitialValues(i,'C')
		Pn = TwoCompartmentInitialValues(i,'N')
		P = [Pc[2],Pn[0]*Pc[2],Pn[1]+Pn[3]] ;[V1,K21*V1,K2]
	endif
	fit = ToftsFitResidue(time, curve,aif, pars = P, status=status $
		,mp=mp, noderivative=noderivative, quiet = quiet, constrained=constrained)

	if arg_present(Pc) then Pc = [P[1],P[0],P[1]/P[2]] ;[F21,V1,V2]

	if arg_present(AIC) then begin
		n = n_elements(Curve)
		AIC = n*alog(total((Curve-Fit)^2)/n) + 2D*(1+n_elements(P))
	endif
	return, Fit
end

function FitToftsDelay, time, roicurve, aif,Delay=Delay, Pars=P,AIC=AIC,Init=i,Convective=Pc,mp=mp,noderivative=noderivative,quiet=quiet,constrained=constrained, nodelay=nodelay

	if n_elements(Delay) eq 0 then Delay = findgen(16)

	if n_elements(i) ne 0 then begin

		Pc = TwoCompartmentInitialValues(i,'C')
		Pn = TwoCompartmentInitialValues(i,'N')
		P = [Pc[2],Pn[0]*Pc[2],Pn[1]+Pn[3]] ;[V1,K21*V1,K2]
	endif

	if keyword_set(nodelay) then begin
		Fit =	FitTofts(Time,RoiCurve,Aif,Pars=P,AIC=AIC,Convective=Pc,mp=mp,noderivative=noderivative,quiet=quiet,constrained=constrained)
		Delay = 0.0
		return, Fit
	endif

	nDel = n_elements(Delay)
	Err = dindgen(nDel)

	for i=0L,nDel-1 do begin

		Pi = P
		Fit = FitTofts(Time,RoiCurve,ShiftAif(Aif,Time,Delay[i]),Pars=Pi,mp=mp,noderivative=noderivative,quiet=quiet,constrained=constrained)
		Err[i] = total((RoiCurve-Fit)^2)
	endfor

	tmp = min(Err,i)
	Delay = Delay[i]
	Fit = FitTofts(Time,RoiCurve,ShiftAif(Aif,Time,Delay),Pars=P,AIC=AIC,Convective=Pc,mp=mp,noderivative=noderivative,quiet=quiet,constrained=constrained)

	AIC = AIC + 2D

	return, Fit
end