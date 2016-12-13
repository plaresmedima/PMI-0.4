Pro FitSingleInletUptakeModel, X, P, F, Fi

  n = n_elements(X)/2

  F1  = ExpConvolution(P[0],X,Der=D1)
  F2  = IntVector(X[0:n-1],X[n:2*n-1])
  F   = P[1]*F1 + P[2]*F2
  F0  = P[1]*D1

  Fi = [[F0],[F1],[F2]]
end


function FitSingleInletUptake,Time,Curve,Aif,Pars=P, AIC=AIC,Init=i,Convective=Pc, mp =mp, status = status $
                              , noderivative = noderivative, quiet=quiet $
                              ,printerrors=printerrors $
                              , covar = covar $
                              , ac = ac, constrained=constrained

  if n_elements(i) ne 0 then begin

     P = TwoCompartmentInitialValues(i,'K')
     P = [P[0],P[2],P[4]] 	;[E21,T1,F10]
  endif

  P = [1/P[1],P[2]*(1-P[0]),P[2]*P[0]] ;[K1,F10*(1-E21),F10*E21]
  IF keyword_set(mp) THEN BEGIN
  	parinfo = replicate({limited:[0,0], limits:[0.D,0]}, 3)
  	if keyword_set(constrained) then parinfo[*].limited[0]=1
     fit = mpcurvefit([time,aif], curve, 1+curve*0, P, FUNCTION_name='FitSingleInletUptakeModel' $
                      ,status=status,errmsg=errmsg, quiet=quiet $
                      ,noderivative=noderivative $
                      ,covar=covar, parinfo=parinfo)

     IF (status LE 0) AND keyword_set(printerrors)  THEN print, 'MPFit failed with ', errmsg
  ENDIF ELSE BEGIN
     Fit = CurveFit([Time,Aif],Curve,1+Curve*0,P,function_name='FitSingleInletUptakeModel',status=status $
                    ,noderivative=noderivative)
     IF (status NE 0)AND keyword_set(printerrors)  THEN print, 'Curvefit failed '
  ENDelse
  P = [P[2]/(P[1]+P[2]),1/P[0],P[1]+P[2]] ;[E21,T1,F10]

  if arg_present(Pc) then Pc = [P[0]*P[2],P[1]*P[2],P[2]] ;[F21,V1,F10]

  IF n_elements(ac) NE 0 THEN BEGIN
     lag = indgen(n_elements(curve)-1)
     ;ac = a_correlate(curve-fit,lag,/covariance)
     ac = a_correlate(curve-fit,lag)
     ac = total(ac[1:*]^2)
  ENDIF

  if arg_present(AIC) ne 0 then begin
 	n = n_elements(Curve)
	AIC = n*alog(total((Curve-Fit)^2)/n) + 2D*(1+n_elements(P))
  endif

  return, Fit
end

function FitSingleInletUptakeDelay, time,curve,aif,Delay=Delay,Pars=P,Init=i,AIC=AIC,Convective=Pc,mp=mp,noderivative=noderivative,quiet=quiet, constrained = constrained, nodelay=nodelay

	if n_elements(Delay) eq 0 then Delay = findgen(10)

  	if n_elements(i) ne 0 then begin

     	P = TwoCompartmentInitialValues(i,'K')
     	P = [P[0],P[2],P[4]] 	;[E21,T1,F10]
  	endif

	if keyword_set(nodelay) then begin
		Fit =	FitSingleInletUptake(Time,curve,Aif,Pars=P,Convective=Pc,AIC=AIC,mp=mp,noderivative=noderivative,quiet=quiet,constrained = constrained)
		Delay = 0.0
		return, Fit
	endif

	nDel = n_elements(Delay)
	Err = dindgen(nDel)

	for i=0L,nDel-1 do begin

		Pi = P
		Fit = FitSingleInletUptake(Time,curve,ShiftAif(Aif,Time,Delay[i]),Pars=Pi,mp=mp,noderivative=noderivative,quiet=quiet,constrained = constrained)
		Err[i] = total((curve-Fit)^2)
	endfor

	tmp = min(Err,i)
	Delay = Delay[i]
	Fit = FitSingleInletUptake(Time,curve,ShiftAif(Aif,Time,Delay),Pars=P,AIC=AIC,Convective=Pc,mp=mp,noderivative=noderivative,quiet=quiet,constrained = constrained)

	AIC = AIC + 2D

	return, Fit
end