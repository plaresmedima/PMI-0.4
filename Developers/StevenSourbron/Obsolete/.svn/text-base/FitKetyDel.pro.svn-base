
Pro KetyModel, X, P, F, Fi

  F0 = ExpConvolution(P[1],X,Der=D1)

  F 	= P[0]*F0
  F1 	= P[0]*D1

  Fi = [[F0],[F1]]
end

function FitKety, time, curve, aif, Pars=P, AIC=AIC, Init=i,ChiSq=ChiSq,Iter=Iter $
                  , mp = mp, noderivative = noderivative, ac=ac, quiet=quiet $
                  , constrained = constrained

    if n_elements(i) ne 0 then begin

		Pc = TwoCompartmentInitialValues(i,'C')
		P = [Pc[4],Pc[2]+Pc[3]] ;[F,V]
	endif

  P = [P[0],P[0]/P[1]]	;[F,1/T]

;  P = double(P)
;  time = double(time)
;  aif = double(aif)
;  curve = double(curve)

  IF keyword_set(mp) THEN BEGIN
  	parinfo = replicate({limited:[0,0], limits:[0.D,0]}, 2)
  	if keyword_set(constrained) then parinfo[*].limited[0]=1
     fit = mpcurvefit([time,aif],curve,1+curve*0,P,FUNCTION_name='KetyModel' $
                      ,noderivative=noderivative, status=status, quiet=quiet,parinfo=parinfo)
  ENDIF ELSE begin
     Fit = curveFit([time,aif],curve,1+curve*0,P,function_name='KetyModel' $
                         ,ChiSq=ChiSq,Iter=Iter,noderivative=noderivative)
  ENDELSE

  IF n_elements(ac) NE 0 THEN BEGIN
     lag = indgen(n_elements(curve)-1)
     ;ac = a_correlate(curve-fit,lag,/covariance)
     ac = a_correlate(curve-fit,lag)
     ac = total(ac[1:*]^2)

  ENDIF

	if arg_present(AIC) then begin
		n = n_elements(Curve)
		AIC = n*alog(total((Curve-Fit)^2)/n) + 2D*(1+n_elements(P))
	endif

  P = [P[0],P[0]/P[1]] ;[F,V]

  return, Fit
end




function FitKetyDel, time, roicurve, aif,Delay=Delay, Pars=P,AIC=AIC,Init=i,mp=mp,noderivative=noderivative,quiet=quiet,constrained=constrained,nodelay=nodelay

 	if n_elements(Delay) eq 0 then Delay = findgen(21)

    if n_elements(i) ne 0 then begin

		Pc = TwoCompartmentInitialValues(i,'C')
		P = [Pc[4],Pc[2]+Pc[3]] ;[F10,V1+V2]
	endif

	if keyword_set(nodelay) then begin
		Fit =FitKety(Time,RoiCurve,Aif,Pars=P,AIC=AIC,mp=mp,noderivative=noderivative,quiet=quiet,constrained=constrained)
		Delay = 0.0
		return, Fit
	endif

	nDel = n_elements(Delay)
	Err = dindgen(nDel)

	for i=0L,nDel-1 do begin

		Pi = P
		Fit = FitKety(Time,RoiCurve,ShiftAif(Aif,Time,Delay[i]),Pars=Pi,mp=mp,noderivative=noderivative,quiet=quiet,constrained=constrained)
		Err[i] = total((RoiCurve-Fit)^2)
	endfor

	tmp = min(Err,j)
	Delay = Delay[j]
	Fit = FitKety(Time,RoiCurve,ShiftAif(Aif,Time,Delay),Pars=P,AIC=AIC,mp=mp,noderivative=noderivative,quiet=quiet,constrained=constrained)

	AIC = AIC + 2D

	return, Fit
end