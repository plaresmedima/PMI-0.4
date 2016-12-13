function FitDualInletDelay, Model,time,curve,aif,vif,Delay=Delay,VifDelay=VifDelay,Pars=P,AIC=AIC,_EXTRA=e

	if n_elements(Delay) le 1 then Delay = findgen(10)

	nDel = n_elements(Delay)
	Err = dindgen(nDel)

	for i=0L,nDel-1 do begin
		Pi = P & Ai = Aif & Vi = Vif
		if keyword_set(VifDelay) then Vi=ShiftAif(Vi,Time,Delay[i]) else Ai=ShiftAif(Ai,Time,Delay[i])
		Fit = Call_Function(Model,Time,curve,Ai,Vi,Pars=Pi,_EXTRA=e)
		Err[i] = total((curve-Fit)^2)
	endfor

	tmp = min(Err,i)
	Delay = Delay[i]
	Ai = Aif & Vi = Vif
	if keyword_set(VifDelay) then Vi=ShiftAif(Vi,Time,Delay) else Ai=ShiftAif(Ai,Time,Delay)
	Fit = Call_Function(Model,Time,curve,Ai,Vi,Pars=P,AIC=AIC,_EXTRA=e)

	AIC = AIC + 2D

	return, Fit
end