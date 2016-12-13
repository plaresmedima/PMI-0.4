Pro TwoCompartmentFitBiexp, X, P, F, Fi

  F2 = ExpConvolution(P[0],X,Der=D0)
  F3 = ExpConvolution(P[1],X,Der=D1)

  F 	= P[2]*F2 + P[3]*F3
  F0 	= P[2]*D0
  F1 	= P[3]*D1

  Fi = [[F0],[F1],[F2],[F3]]
end


function TwoCompartmentFitResidue, time, Curve, aif, Pars=P  $
                                   , status = status $
                                   , mp = mp $
                                   , noderivative = noderivative $
                                   , weights = weights $
                                   , covar = covar, quiet=quiet $
                                   , constrained = constrained


  ;; P = [K+,K-,K01,F10]

  Fp = P[3]*(P[2]-P[1])/(P[0]-P[1])
  P = [P[0:1],Fp,P[3]-Fp]
  P=double(P)
  time = double(time)
  aif = double(aif)
  curve = double(curve)


  ;; P = [K+,K-,F+,F-]
  IF n_elements(weights) EQ 0 THEN weights = curve*0 + 1
  IF keyword_set(mp) THEN BEGIN
   parinfo = replicate({limited:[0,0], limits:[0.D,0]}, 4)
  	 if keyword_set(constrained) then parinfo[*].limited[0]=1
     Fit = mpCurveFit([time,aif],Curve,weights,P,function_name='TwoCompartmentFitBiexp' $
                     , status =status $
                      , noderivative=noderivative, quiet=quiet $
                     ,covar=covar,parinfo=parinfo)
  ENDIF ELSE BEGIN
     Fit = CurveFit([time,aif],Curve,weights,P,function_name='TwoCompartmentFitBiexp' $
                    , status =status $
                   , noderivative = noderivative)
     IF (status NE 0) AND (NOT keyword_set(quiet)) THEN print, 'Curvefit failed with status ', status
  ENDELSE

  Kp = max(P[0:1],i)
  Km = P[1-i]
  Fp = P[2+i]
  Fm = P[3-i]

  F10 = Fp+Fm
  K01 = (Fp/F10)*(Kp-Km) + Km

  P = [Kp,Km,K01,F10]

  return, Fit
end


Function TwoCompartmentFit,Time,Curve,Aif,E12,Pars=P,Init=i,Convective=Pc,Kinetic=Pk,Positive=Pos $
                           , status = status $
                           , mp=mp $
                           , noderivative = noderivative $
                           , covar = covar $
                           , quiet = quiet, aic = aic $
                           , constrained = constrained

  if n_elements(i) ne 0 then begin
     P = TwoCompartmentInitialValues(i,'I')
	 P = [P[0:2],P[4]]          ;[K+,K-,K01,F10]
  endif

  Fit = TwoCompartmentFitResidue(Time,Curve,Aif,Pars=P, status=status $
                                 , mp = mp, noderivative = noderivative $
                                ,covar = covar, quiet = quiet, constrained = constrained)
  if arg_present(Pk) then Pk = TwoCompartmentRepresentation([P[0:2],E12,P[3],0],'I','K',Positive=Pos)
  if arg_present(Pc) then Pc = TwoCompartmentRepresentation([P[0:2],E12,P[3],0],'I','C',Positive=Pos)

  if arg_present(aic) ne 0 then begin
 	n = n_elements(Curve)
	AIC = n*alog(total((Curve-Fit)^2)/n) + 2D*(1+n_elements(P))
  endif

  return, Fit
end


function TwoCompartmentDelayFit_Indirect,time,curve,aif,E12,Delay=Delay,Pars=P,AIC=AIC,Init=i,Convective=Pc,Kinetic=Pk,Positive=Pos,mp=mp,noderivative=noderivative,quiet=quiet,constrained=constrained,nodelay=nodelay

	if n_elements(Delay) eq 0 then Delay = findgen(16)

  	if n_elements(i) ne 0 then begin
     	P = TwoCompartmentInitialValues(i,'I')
	 	P = [P[0:2],P[4]]          ;[K+,K-,K01,F10]
  	endif

	if keyword_set(nodelay) then begin
		Fit =	TwoCompartmentFit(Time,Curve,Aif,E12,Pars=P,AIC=AIC,Convective=Pc,Kinetic=Pk,Positive=Pos,mp=mp,noderivative=nderivative,quiet=quiet,constrained=constrained)
		Delay = 0.0
		return, Fit
	endif

	nDel = n_elements(Delay)
	Err = dindgen(nDel)

	for i=0L,nDel-1 do begin

		Pi = P
		Fit = TwoCompartmentFit(Time,Curve,ShiftAif(Aif,Time,Delay[i]),E12,Pars=Pi,Positive=Pos,mp=mp,noderivative=noderivative,quiet=quiet,constrained=constrained)
		Err[i] = total((Curve-Fit)^2)
	endfor

	tmp = min(Err,i)
	Delay = Delay[i]
	Fit = TwoCompartmentFit(Time,Curve,ShiftAif(Aif,Time,Delay),E12,Pars=P,AIC=AIC,Convective=Pc,Kinetic=Pk,Positive=Pos,mp=mp,noderivative=nderivative,quiet=quiet,constrained=constrained)

	AIC = AIC + 2D

	return, Fit
end