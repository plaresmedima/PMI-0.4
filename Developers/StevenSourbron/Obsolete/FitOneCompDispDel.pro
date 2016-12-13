;C = CA *  V (exp(-t/T)-exp(-t/TA))/(T-TA)

Pro OneCompDisp, X, P, F, Fi

	;P=[V,T,TA]

	X1 = ExpConvolution(1/P[1],X,Der=D1)
	X2 = ExpConvolution(1/P[2],X,Der=D2)

	F0 	= (X1-X2)/(P[1]-P[2])
	F 	= P[0]*F0
	F1 	= P[0]*D1/(P[1]-P[2])
	F2 	= P[0]*D2/(P[2]-P[1])

	Fi = [[F0],[F1],[F2]]
end

function FitOneCompDisp, time, curve, aif, Pars=P,AIC=AIC ,Init=i,ChiSq=ChiSq,Iter=Iter $
                  , mp = mp, noderivative = noderivative, quiet=quiet,constrained=constrained

    if n_elements(i) ne 0 then begin

		Pc = TwoCompartmentInitialValues(i,'C')
		P = [Pc[4],Pc[2]+Pc[3],2] ;[F,V,TA]
	endif

	P = [P[1],P[1]/P[0],P[2]] ;[V,T,TA]

  IF keyword_set(mp) THEN BEGIN
    parinfo = replicate({limited:[0,0], limits:[0.D,0]}, 3)
  	if keyword_set(constrained) then parinfo[*].limited[0]=1
     fit = mpcurvefit([time,aif],curve,1+curve*0,P,FUNCTION_name='OneCompDisp' $
                      ,noderivative=noderivative, status=status, quiet=quiet,parinfo=parinfo)
  ENDIF ELSE begin
     Fit = curveFit([time,aif],curve,1+curve*0,P,/double,function_name='OneCompDisp' $
                         ,ChiSq=ChiSq,Iter=Iter)
  ENDELSE

  if arg_present(AIC) ne 0 then begin
 	n = n_elements(Curve)
	AIC = n*alog(total((Curve-Fit)^2)/n) + 2D*(1+n_elements(P))
  endif


	P = [P[0]/P[1],P[0],P[2]] ;[F,V,TA]

	return, Fit
end

function FitOneCompDispDel, time, roicurve, aif,Delay=Delay, Pars=P,AIC=AIC, Init=i, nodelay=nodelay,mp=mp,noderivative=noderivative,quiet=quiet,constrained=constrained

	if n_elements(Delay) eq 0 then Delay = findgen(21)

    if n_elements(i) ne 0 then begin

		Pc = TwoCompartmentInitialValues(i,'C')
		P = [Pc[4],Pc[2]+Pc[3],2] ;[F,V,TA]
	endif

	if keyword_set(nodelay) then begin
		Fit =FitOneCompDisp(Time,Roicurve,Aif,Pars=P,AIC=AIC,mp=mp,noderivative=noderivative,quiet=quiet,constrained=constrained)
		Delay = 0.0
		return, Fit
	endif


	nDel = n_elements(Delay)
	Err = dindgen(nDel)

	for i=0L,nDel-1 do begin

		Pi = P
		Fit = FitOneCompDisp(Time,RoiCurve,ShiftAif(Aif,Time,Delay[i]),Pars=Pi,mp=mp,noderivative=noderivative,quiet=quiet,constrained=constrained)
		Err[i] = total((RoiCurve-Fit)^2)
	endfor

	tmp = min(Err,i)
	Delay = Delay[i]
	Fit = FitOneCompDisp(Time,RoiCurve,ShiftAif(Aif,Time,Delay),Pars=P,AIC=AIC,mp=mp,noderivative=noderivative,quiet=quiet,constrained=constrained)

	AIC = AIC + 2D

	return, Fit
end