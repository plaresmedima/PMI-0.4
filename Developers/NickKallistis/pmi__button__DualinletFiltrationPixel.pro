FUNCTION PMI__Button__Input__DualinletFiltrationPixel, ev $
	,	status = status, Stdy = Stdy $
	,	T1series = T1series, DynSeries = DynSeries,	Roi	= Roi $
	,	time = time, aif = aif, vif = vif $
	,	nb= nb, FA=FA, TR=TR, Relaxivity=Relaxivity

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

	T1series = Stdy->Obj(0,in.T1)
	DynSeries = Stdy->Obj(0,ind[in.ser])
	Roi = Stdy->Obj(1,in.roi)

	TR = DynSeries->GETVALUE('0018'x,'0080'x)
	FA = DynSeries->GETVALUE('0018'x,'1314'x)
	p = PMI__Form(ev.top, Title='Check sequence parameters', [$
		ptr_new({Type:'VALUE', Tag:'TR' , Label:'Repetition Time (msec)', Value:TR}),$
		ptr_new({Type:'VALUE', Tag:'FA' , Label:'Flip Angle (Degrees)', Value:FA}) $
		])
	IF p.cancel THEN return, 0
	TR=p.TR & FA=p.FA

    time = DynSeries->t() - DynSeries->t(0)
    nb = ceil(in.nt/time[1])

    Aif = PMI__RoiCurve(Stdy->DataPath(), DynSeries, Stdy->Obj(1,in.aif), status, cnt=cnt)
    if cnt eq 0 then begin
    	ok = dialog_message(/information,'Arterial ROI is empty!')
    	return, 0
    endif
    Vif = PMI__RoiCurve(Stdy->DataPath(), DynSeries, Stdy->Obj(1,in.vif), status, cnt=cnt)
    if cnt eq 0 then begin
    	ok = dialog_message(/information,'Venous ROI is empty!')
    	return, 0
    endif

    Relaxivity = 3.6 ;value does not matter

    T10 = 1628.66
    S0 = total(Aif[0:nb-1])/nb
    Aif =  Concentration_SPGRESS(Aif, S0, T10, FA, TR, Relaxivity)/(1-in.hct)

    T10 = 1530.22
    S0 = total(Vif[0:nb-1])/nb
    Vif =  Concentration_SPGRESS(Vif, S0, T10, FA, TR, Relaxivity)/(1-in.hct)

	return, 1

END

pro PMI__Button__Event__DualinletFiltrationPixel, ev

	IF NOT PMI__Button__Input__DualinletFiltrationPixel(ev $
	,	status = status, Stdy = Stdy $
	,	T1series = T1series, DynSeries = DynSeries,	Roi	= Roi $
	,	time = time, aif = aif, vif = vif $
	,	nb= nb, FA=FlipAngle, TR=TR, Relaxivity=Relaxivity $
	) THEN RETURN

	PMI__Message, status, 'Preparing calculation..'

	d = DynSeries->d()

	FA 	= fltarr(d[0]*d[1],d[2])
	FV 	= fltarr(d[0]*d[1],d[2])
	VE 	= fltarr(d[0]*d[1],d[2])
	UF  = fltarr(d[0]*d[1],d[2])
	FT  = fltarr(d[0]*d[1],d[2])
	AF	= fltarr(d[0]*d[1],d[2])
	MT	= fltarr(d[0]*d[1],d[2])
	UR	= fltarr(d[0]*d[1],d[2])
	ER	= fltarr(d[0]*d[1],d[2])
	AIC	= fltarr(d[0]*d[1],d[2])

    for j=0L,d[2]-1 do begin

		slice = strcompress(j,/remove_all) +'/' + strcompress(d[2]-1,/remove_all)
		PMI__Message, status, 'Fitting slice ' + slice

		Mask = Roi -> Read(Stdy->DataPath(),j,0)
		for k=1L, Roi->d(3)-1 do $
			Mask = Mask or Roi -> Read(Stdy->DataPath(),j,k)
		Ind = where(Mask eq 1, cnt)

		if cnt gt 0 then begin

			T10 = T1Series -> Read(Stdy->DataPath(),j,-1)
			Curves = DynSeries -> Read(Stdy->DataPath(),j,-1)
			Curves = REFORM(Curves, d[0]*d[1], d[3], /overwrite)

			for i=0L, cnt-1 do begin

				PMI__Message, status, 'Fitting slice ' + slice + ' ( '+strcompress(100*i/(cnt-1.0),/remove_all)+' %)'

				k = Ind[i]

				Signal = reform(Curves[k,*])
				S0 = total(signal[0:nb-1])/nb
				Conc = Concentration_SPGRESS(Signal, S0, T10[k], FlipAngle, TR, Relaxivity)
				Pars = [0.2*50/6000D,0.8*50/6000D,0.2,0.2,0.2] ;P = [FA,FV,VE,EI,EB]
				Fit = FitDualInlet('Filtration', Time, Aif, Vif, Conc, Pars, LIMITED_ABOVE=[1,1,1,1,1], LIMITS_ABOVE=[1000/6000D,1000/6000D,1,1D,1], $
			  		AKAIKE_ERROR=aic_i, /POSITIVITY, /NODERIVATIVE)

				FA[k,j] 	= 6000.0*Pars[0]
				FV[k,j] 	= 6000.0*Pars[1]
				VE[k,j]    	= 100D*Pars[2]
				UF[k,j]		= 100.0*Pars[3]
				FT[k,j]		= 6000.0*(Pars[0]+Pars[1])
				AF[k,j]    	= 100*Pars[0]/(Pars[0]+Pars[1])
				MT[k,j] 	= Pars[2]*(1-Pars[3])/(Pars[0]+Pars[1])
				UR[k,j]		= 6000.0*(Pars[0]+Pars[1])*Pars[3]/(1-Pars[3])
				ER[k,j]		= 6000.0*(Pars[0]+Pars[1])*Pars[4]/(1-Pars[4])
				AIC[k,j] 	= AIC_i

			endfor
		endif
	endfor


	PMI__Message, status, 'Saving Results'

	Domain 	= {z:DynSeries->z(), t:DynSeries->t(0), m:DynSeries->m()}

	Sfa = Stdy->New('SERIES', Domain=Domain, Data=remove_inf(FA),	Name='2I2CFM - Arterial Flow (ml/100ml/min)')
	Sfv = Stdy->New('SERIES', Domain=Domain, Data=remove_inf(FV),	Name='2I2CFM - Venous Flow (ml/100ml/min)')
	Sve = Stdy->New('SERIES', Domain=Domain, Data=remove_inf(VE),	Name='2I2CFM - Extracellular Volume (ml/100ml)')
	Suf = Stdy->New('SERIES', Domain=Domain, Data=remove_inf(UF),	Name='2I2CFM - Uptake Fraction (%)')
	Sft = Stdy->New('SERIES', Domain=Domain, Data=remove_inf(FT),	Name='2I2CFM - Total Flow (ml/100ml/min)')
	Saf = Stdy->New('SERIES', Domain=Domain, Data=remove_inf(AF),	Name='2I2CFM - Arterial Flow Fraction %')
	Smt = Stdy->New('SERIES', Domain=Domain, Data=remove_inf(MT),	Name='2I2CFM - Mean Transit Time (sec)')
	Sup = Stdy->New('SERIES', Domain=Domain, Data=remove_inf(UR),	Name='2I2CFM - Uptake Rate (ml/100ml/min')
	Ser = Stdy->New('SERIES', Domain=Domain, Data=remove_inf(ER),	Name='2I2CFM - Excretion Rate (ml/100ml/min')
	Saic = Stdy->New('SERIES', Domain=Domain, Data=remove_inf(AIC),	Name='2I2CFM - Akaike Error')


	Sfa->Trim, [0,100]
	Sfv->Trim, [0,300]
	Sve->Trim, [0,50]
	Suf->Trim, [0,50]
	Sft->Trim, [0,500]
	Saf->Trim, [0,50]
	Smt->Trim, [0,50]
	Sup->Trim, [0,30]
	Ser->Trim, [0,10]
	Saic->Trim, [-3000,0]

	PMI__Control, ev.top, /refresh
	return:PMI__Message, status
end

pro PMI__Button__Control__DualinletFiltrationPixel, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 2)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__DualinletFiltrationPixel, parent,value=value, separator=separator

	if n_elements(value) eq 0 then value = 'Fit dual input 2-compartment model (Pixel)'

	id = widget_button(parent $
	,	value 		= value	$
	,	event_pro 	= 'PMI__Button__Event__DualinletFiltrationPixel'	$
	,	pro_set_value 	= 'PMI__Button__Control__DualinletFiltrationPixel' $
	, 	separator 	= separator	)

	return, id

end
