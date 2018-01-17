pro PMI__Button__Event__DualInletUptakePixel, ev

	if not DualInletPerfusionAnalysis__input($
		ev.top $
	,	Stdy		= Stdy $
	,	status 		= status $
	,	time 		= time $
	,	pixelcurve 	= p $
	,	Roi			= Roi $
	,	aif			= aif $
	,	vif 		= vif $
	,	ev = ev $
	) then goto, return

	PMI__Message, status, 'Preparing calculation..'

	d = Roi->d()
	n = mult(d[0:2])
	FA 	= fltarr(n)
	FV 	= fltarr(n)
	AF  = fltarr(n)
	MT  = fltarr(n)
	VL	= fltarr(n) ;extracellular volume
	UP	= fltarr(n) ;uptake fraction
	UF	= fltarr(n) ;uptake flow

   i=0L
   ind = Roi -> Where(Stdy->DataPath(),t=Roi->t(i),n=n)
   while n eq 0 do begin
    	i=i+1
    	ind = Roi -> Where(Stdy->DataPath(),t=Roi->t(i),n=n)
   endwhile

	for i=0L,n-1 do begin

		PMI__Message, status, 'Fitting ' + Roi->name() + ' pixels to Dual-inlet 2-compartment uptake model', 	i/(n-1.0)

		;Pars = [0.2*50/6000.,0.8*50/6000.,20.]
		Pars = [0.2*50/6000D,0.8*50/6000.,20.,0.2]
		;p[i,*] = FitDualInletCompartment(time,reform(p[i,*]),aif,vif,Pars=Pars,/quiet)
		Fit = FitDualInletUptake(time,reform(p[i,*]),aif,vif,Pars=Pars,AIC=AIC,/noderivative,/quiet,/constrained)

		FA[ind[i]] 	= 6000.0*Pars[0]
		FV[ind[i]] 	= 6000.0*Pars[1]
		AF[ind[i]]  = 100*FA[ind[i]]/(FA[ind[i]]+FV[ind[i]])
		MT[ind[i]] 	= Pars[2]
		VL[ind[i]]	= 100*(Pars[0]+Pars[1])*Pars[2]/(1-Pars[3])
		UP[ind[i]]	= 100*Pars[3]
		UF[ind[i]]	= 6000*(Pars[0]+Pars[1])*Pars[3]/(1-Pars[3])

	endfor


	PMI__Message, status, 'Saving Results'

	Domain 	= {z:Roi->z(), t:Roi->t(0), m:Roi->m()}

	S = Stdy->New('SERIES', Domain=Domain, Data=FA,	Name='2-inlet 2-compartment model - Arterial Flow (ml/100ml/min)')
	S = Stdy->New('SERIES', Domain=Domain, Data=FV,	Name='2-inlet 2-compartment model - Venous Flow (ml/100ml/min)')
	S = Stdy->New('SERIES', Domain=Domain, Data=AF,	Name='2-inlet 2-compartment model - Arterial Flow Fraction %')
	S = Stdy->New('SERIES', Domain=Domain, Data=MT,	Name='2-inlet 2-compartment model - Mean Transit Time (sec)')
	S = Stdy->New('SERIES', Domain=Domain, Data=VL,	Name='2-inlet 2-compartment model - Extracellular Volume (ml/100ml)')
	S = Stdy->New('SERIES', Domain=Domain, Data=UP,	Name='2-inlet 2-compartment model - Uptake Fraction %')
	S = Stdy->New('SERIES', Domain=Domain, Data=UF,	Name='2-inlet 2-compartment model - Uptake Flow (ml/100ml/min)')


	PMI__Control, ev.top, /refresh
	return:PMI__Message, status
end

pro PMI__Button__Control__DualInletUptakePixel, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 1)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__DualInletUptakePixel, parent,value=value, separator=separator

	if n_elements(value) eq 0 then value = 'Fit dual input 2-compartment model (Pixel)'

	id = widget_button(parent $
	,	value 		= value	$
	,	event_pro 	= 'PMI__Button__Event__DualInletUptakePixel'	$
	,	pro_set_value 	= 'PMI__Button__Control__DualInletUptakePixel' $
	, 	separator 	= separator	)

	return, id

end
