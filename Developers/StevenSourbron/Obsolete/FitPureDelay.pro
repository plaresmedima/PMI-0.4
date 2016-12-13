function FitPureDelay, time,roicurve,aif,Delay=Tdel,Pars=Pars,Err=err

	Pars = fltarr(1)
	Pars[0] = max(roicurve)/max(AIF)
	curve = roicurve/Pars[0]

	nT = n_elements(Tdel)
	Tmax = max(Tdel)
	Err = findgen(nT)

	ti = where(Time lt max(Time)-Tmax)

	for i=0L,nT-1 do begin

		ShAif = ShiftAif(Aif,Time,Tdel[i])
		Err[i] = total((curve[ti]-ShAif[ti])^2)
	endfor

	tmp = min(Err,i)
	Tdel = Tdel[i]

	return, Pars[0]*ShiftAif(Aif,Time,Tdel)
end