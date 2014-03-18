function FitDualInletDualDelay,Model,time,curve,aif,vif,Delay=Delay,Pars=P,AIC=AIC,_EXTRA=e

	if n_elements(Delay) le 1 then Delay = findgen(10)

	nDel = n_elements(Delay)
	Err = dindgen(nDel,nDel)

	for i=0L,nDel-1 do begin
	for j=0L,nDel-1 do begin

		Pi = P
		Fit = Call_Function(Model,Time,curve,ShiftAif(Aif,Time,Delay[i]),ShiftAif(Vif,Time,Delay[j]),Pars=Pi, _EXTRA=e)
		Err[i,j] = total((curve-Fit)^2)

	endfor
	endfor

	tmp = min(Err,i)
	r = reform_ind([nDel,nDel],ind=i)
	Delay = [Delay[r[0]],Delay[r[1]]]
	Fit = Call_Function(Model,Time,curve,ShiftAif(Aif,Time,Delay[0]),ShiftAif(Vif,Time,Delay[1]),Pars=P,AIC=AIC,_EXTRA=e)

	AIC = AIC + 4D

	return, Fit
end