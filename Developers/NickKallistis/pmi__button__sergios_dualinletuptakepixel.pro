FUNCTION PMI__Button__Input__Sergios_DualInletUptakePixel, ev $
	,	Stdy		= Stdy $
	,	status 		= status $
	,	time 		= time $
	,	pixelcurve 	= pcurve $
	,	Roi			= Roi $
	,	aif			= aif $
	,	vif 		= vif

	PMI__Info, ev.top, Status=status, Stdy=Stdy

    Series = Stdy->Names(0,ns,DefDim=3,ind=ind,sel=sel)
    Regions = Stdy->names(1,nr)

	in = PMI__Form(ev.top, Title='Perfusion analysis setup', [$
		ptr_new({Type:'DROPLIST',Tag:'T1', Label:'Precontrast T1-map (msec)', Value:Stdy->Names(0), Select:Stdy->sel(0)}), $
		ptr_new({Type:'DROPLIST',Tag:'ser', Label:'Dynamic series', Value:Series, Select:sel}), $
		ptr_new({Type:'DROPLIST',Tag:'aif', Label:'Arterial Region', Value:Regions, Select:Stdy->sel(1)}), $
		ptr_new({Type:'DROPLIST',Tag:'vif', Label:'Venous Region', Value:Regions, Select:Stdy->sel(1)}), $
		ptr_new({Type:'DROPLIST',Tag:'roi', Label:'Tissue Region', Value:Regions, Select:Stdy->sel(1)}), $
		ptr_new({Type:'VALUE'	,Tag:'nt' , Label:'Length of baseline (sec)', Value:5.0}),$
		ptr_new({Type:'VALUE'	,Tag:'hct', Label:'Patients hematocrit', Value:0.45})])
	IF in.cancel THEN return, 0

	Series = Stdy->Obj(0,ind[in.ser])
    time = Series->t() - Series->t(0)
    nb = ceil(in.nt/time[1])
	TR = series->GETVALUE('0018'x,'0080'x)
	FA = series->GETVALUE('0018'x,'1314'x)
	p = PMI__Form(ev.top, Title='Check sequence parameters', [$
		ptr_new({Type:'VALUE', Tag:'TR' , Label:'Repetition Time (msec)', Value:TR}),$
		ptr_new({Type:'VALUE', Tag:'FA' , Label:'Flip Angle (Degrees)', Value:FA}) $
		])
	IF p.cancel THEN return, 0

    Roi = Stdy->Obj(1,in.roi)
	RoiName = Roi->Name()

    Aif = PMI__RoiCurve(Stdy->DataPath(), Series, Stdy->Obj(1,in.aif), status, cnt=cnt)
    if cnt eq 0 then begin
    	ok = dialog_message(/information,'Arterial ROI is empty!')
    	return, 0
    endif
    Vif = PMI__RoiCurve(Stdy->DataPath(), Series, Stdy->Obj(1,in.vif), status, cnt=cnt)
    if cnt eq 0 then begin
    	ok = dialog_message(/information,'Venous ROI is empty!')
    	return, 0
    endif
	pcurve = PMI__PixelCurve(Stdy->DataPath(), Series, Roi, status, cnt=cnt)
    if cnt eq 0 then begin
    	ok = dialog_message(/information,'Tissue ROI is empty!')
    	return, 0
    endif

    T1series = Stdy->Obj(0,in.T1)
    Relaxivity = 3.6 ;value does not matter

    T10 = PMI__RoiValues(Stdy->DataPath(), T1Series, Stdy->Obj(1,in.aif), status, cnt=cnt)
    T10 = total(T10)/cnt
    S0 = total(Aif[0:nb-1])/nb
    Aif =  Concentration_SPGRESS(Aif, S0, T10, p.FA, p.TR, Relaxivity)/(1-in.hct)

    T10 = PMI__RoiValues(Stdy->DataPath(), T1Series, Stdy->Obj(1,in.vif), status, cnt=cnt)
    T10 = total(T10)/cnt
    S0 = total(Vif[0:nb-1])/nb
    Vif =  Concentration_SPGRESS(Vif, S0, T10, p.FA, p.TR, Relaxivity)/(1-in.hct)

	np = n_elements(pcurve[*,0])
	T10 = PMI__RoiValues(Stdy->DataPath(), T1Series, Roi, status, cnt=np)
	for i=0L,np-1 do begin
		PMI__Message, status, 'Calculating concentrations ', i/(np-1E)
		S0 = total(pcurve[i,0:nb-1])/nb
		pcurve[i,*] = Concentration_SPGRESS(pcurve[i,*], S0, T10[i], p.FA, p.TR, Relaxivity)
	endfor

	return, 1

END

pro PMI__Button__Event__Sergios_DualInletUptakePixel, ev

	IF NOT PMI__Button__Input__Sergios_DualInletUptakePixel(ev $
	,	Stdy		= Stdy $
	,	status 		= status $
	,	time 		= time $
	,	pixelcurve 	= p $
	,	Roi			= Roi $
	,	aif			= aif $
	,	vif 		= vif $
	) THEN RETURN

	PMI__Message, status, 'Preparing calculation..'

	d = Roi->d()
	n = mult(d[0:2])
	FA 	= fltarr(n)
	FV 	= fltarr(n)
	FT 	= fltarr(n)
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

		Pars = [0.2*50/6000D,0.8*50/6000.,20.,0.2]
		Fit = FitDualInletUptake(time,reform(p[i,*]),aif,vif,Pars=Pars,/noderivative,/quiet,/constrained)

		FA[ind[i]] 	= 6000.0*Pars[0]
		FV[ind[i]] 	= 6000.0*Pars[1]
		FT[ind[i]]	= 6000.0*(Pars[0]+Pars[1])
		AF[ind[i]]  = 100*FA[ind[i]]/(FA[ind[i]]+FV[ind[i]])
		MT[ind[i]] 	= Pars[2]
		VL[ind[i]]	= 100*(Pars[0]+Pars[1])*Pars[2]/(1-Pars[3])
		UP[ind[i]]	= 100*Pars[3]
		UF[ind[i]]	= 6000*(Pars[0]+Pars[1])*Pars[3]/(1-Pars[3])

	endfor


	PMI__Message, status, 'Saving Results'

	Domain 	= {z:Roi->z(), t:Roi->t(0), m:Roi->m()}

	Sfa = Stdy->New('SERIES', Domain=Domain, Data=remove_inf(FA),	Name='2-inlet 2-compartment model - Arterial Flow (ml/100ml/min)')
	Sfv = Stdy->New('SERIES', Domain=Domain, Data=remove_inf(FV),	Name='2-inlet 2-compartment model - Venous Flow (ml/100ml/min)')
	Sft = Stdy->New('SERIES', Domain=Domain, Data=remove_inf(FT),	Name='2-inlet 2-compartment model - Total Flow (ml/100ml/min)')
	Saf = Stdy->New('SERIES', Domain=Domain, Data=remove_inf(AF),	Name='2-inlet 2-compartment model - Arterial Flow Fraction %')
	Smt = Stdy->New('SERIES', Domain=Domain, Data=remove_inf(MT),	Name='2-inlet 2-compartment model - Mean Transit Time (sec)')
	Svl = Stdy->New('SERIES', Domain=Domain, Data=remove_inf(VL),	Name='2-inlet 2-compartment model - Extracellular Volume (ml/100ml)')
	Sup = Stdy->New('SERIES', Domain=Domain, Data=remove_inf(UP),	Name='2-inlet 2-compartment model - Uptake Fraction %')
	Suf = Stdy->New('SERIES', Domain=Domain, Data=remove_inf(UF),	Name='2-inlet 2-compartment model - Uptake Flow (ml/100ml/min)')

	Sfa->Trim, [0,100]
	Sfv->Trim, [0,200]
	Sft->Trim, [0,300]
	Saf->Trim, [0,50]
	Smt->Trim, [0,50]
	Svl->Trim, [0,50]
	Sup->Trim, [0,10]
	Suf->Trim, [0,2]


	PMI__Control, ev.top, /refresh
	return:PMI__Message, status
end

pro PMI__Button__Control__Sergios_DualInletUptakePixel, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 2)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__Sergios_DualInletUptakePixel, parent,value=value, separator=separator

	if n_elements(value) eq 0 then value = 'Fit dual input 2-compartment model (Pixel)'

	id = widget_button(parent $
	,	value 		= value	$
	,	event_pro 	= 'PMI__Button__Event__Sergios_DualInletUptakePixel'	$
	,	pro_set_value 	= 'PMI__Button__Control__Sergios_DualInletUptakePixel' $
	, 	separator 	= separator	)

	return, id

end
