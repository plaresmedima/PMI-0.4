FUNCTION PMI__Button__Input__Jeong_dualinletonecomppixel, ev $
	,	Stdy		= Stdy $
	,	status 		= status $
	,	time 		= time $
	,	pixelcurve 	= pcurve $
	,	Roi			= Roi $
	,	aif			= aif $
	,	vif 		= vif $
	,	hct			= hct

	PMI__Info, ev.top, Status=status, Stdy=Stdy

    Series = Stdy->Names(0,ns,DefDim=3,ind=ind,sel=sel)
    Regions = Stdy->names(1,nr)

	in = PMI__Form(ev.top, Title='Perfusion analysis setup', [$
		ptr_new({Type:'DROPLIST',Tag:'ser', Label:'Dynamic series', Value:Series, Select:sel}), $
		ptr_new({Type:'DROPLIST',Tag:'aif', Label:'Arterial Region', Value:Regions, Select:Stdy->sel(1)}), $
		ptr_new({Type:'DROPLIST',Tag:'vif', Label:'Venous Region', Value:Regions, Select:Stdy->sel(1)}), $
		ptr_new({Type:'DROPLIST',Tag:'roi', Label:'Tissue Region', Value:Regions, Select:Stdy->sel(1)}), $
		ptr_new({Type:'VALUE'	,Tag:'nt' , Label:'Length of baseline (sec)', Value:10.0}),$
		ptr_new({Type:'VALUE'	,Tag:'hct', Label:'Patients hematocrit', Value:0.45})])
	IF in.cancel THEN return, 0

	Series = Stdy->Obj(0,ind[in.ser])
    time = Series->t() - Series->t(0)
    nb = ceil(in.nt/time[1])
    hct = in.hct

    Roi = Stdy->Obj(1,in.roi)
	RoiName = Roi->Name()

	PMI__Message, Status, 'Loading data... '

    Aif = PMI__RoiCurve(Stdy->DataPath(), Series, Stdy->Obj(1,in.aif), id, cnt=cnt)
    if cnt eq 0 then begin
    	ok = dialog_message(/information,'Arterial ROI is empty!')
    	return, 0
    endif
    Vif = PMI__RoiCurve(Stdy->DataPath(), Series, Stdy->Obj(1,in.vif), id, cnt=cnt)
    if cnt eq 0 then begin
    	ok = dialog_message(/information,'Venous ROI is empty!')
    	return, 0
    endif
	pcurve = PMI__PixelCurve(Stdy->DataPath(), Series, Roi, id, cnt=cnt)
    if cnt eq 0 then begin
    	ok = dialog_message(/information,'Tissue ROI is empty!')
    	return, 0
    endif

	T1a = 1.0 / (0.52 * in.hct + 0.38)  ; Lu MRM 2004
	T1v = 1.0 / (0.83 * in.hct + 0.28)  ; Lu MRM 2004
	T1t = 809E/1000 ;sec
	Relaxivity = 3.6E ;Hz/mM

    Aif = LMU__Enhancement(Aif,nb,relative=1)/(1-in.hct) / (relaxivity*T1a)
    Vif = LMU__Enhancement(Vif,nb,relative=1)/(1-in.hct) / (relaxivity*T1v)

	np = n_elements(pcurve[*,0])
	for i=0L,np-1 do begin
		PMI__Message, Status, 'Calculating concentrations ', i/(np-1E)
		pcurve[i,*] = LMU__Enhancement(pcurve[i,*],nb,relative=1) / (relaxivity*T1t)
	endfor

	return, 1

END

pro PMI__Button__Event__Jeong_dualinletonecomppixel, ev

	IF NOT PMI__Button__Input__Jeong_dualinletonecomppixel(ev $
	,	Stdy		= Stdy $
	,	status 		= status $
	,	time 		= time $
	,	pixelcurve 	= p $
	,	Roi			= Roi $
	,	aif			= aif $
	,	vif 		= vif $
	,	hct			= hct $
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
	TA	= fltarr(n)

   i=0L
   ind = Roi -> Where(Stdy->DataPath(),t=Roi->t(i),n=n)
   while n eq 0 do begin
    	i=i+1
    	ind = Roi -> Where(Stdy->DataPath(),t=Roi->t(i),n=n)
   endwhile

	for i=0L,n-1 do begin

		PMI__Message, status, 'Fitting ' + Roi->name() + ' pixels to dual-inlet model', i/(n-1.0)

		Pars = [0.2*50/6000D,0.8*50/6000D,0.2] ;P = [FA,FV,VE+VP]
		Fit = FitDualInlet('Compartment', Time, Aif, Vif, reform(p[i,*]), Pars, LIMITED_ABOVE=[0,0,1], DELAY_PAR=Pd, DELAY_VALUES=[0,20,1], DELAY_WHICH=0, /POSITIVITY, /NODERIVATIVE)

		FA[ind[i]] 	= 6000.0*Pars[0]/hct
		FV[ind[i]] 	= 6000.0*Pars[1]/hct
		FT[ind[i]]	= 6000.0*(Pars[0]+Pars[1])/hct
		AF[ind[i]]  = 100*Pars[0]/(Pars[0]+Pars[1])
		MT[ind[i]] 	= 1D*Pars[2]/(Pars[0]+Pars[1])
		VL[ind[i]]	= 100D*Pars[2]
		TA[ind[i]]  = Pd
	endfor


	PMI__Message, status, 'Saving Results'

	Domain 	= {z:Roi->z(), t:Roi->t(0), m:Roi->m()}

	Sfa = Stdy->New('SERIES', Domain=Domain, Data=remove_inf(FA),	Name='Arterial Blood Flow (ml/100ml/min)')
	Sfv = Stdy->New('SERIES', Domain=Domain, Data=remove_inf(FV),	Name='Venous Blood Flow (ml/100ml/min)')
	Sft = Stdy->New('SERIES', Domain=Domain, Data=remove_inf(FT),	Name='Total Blood Flow (ml/100ml/min)')
	Saf = Stdy->New('SERIES', Domain=Domain, Data=remove_inf(AF),	Name='Arterial Flow Fraction %')
	Smt = Stdy->New('SERIES', Domain=Domain, Data=remove_inf(MT),	Name='Mean Transit Time (sec)')
	Svl = Stdy->New('SERIES', Domain=Domain, Data=remove_inf(VL),	Name='Extracellular Volume (ml/100ml)')
	Sta = Stdy->New('SERIES', Domain=Domain, Data=remove_inf(TA),	Name='Arterial Delay Time (sec)')

	Sfa->Trim, [0,200]
	Sfv->Trim, [0,200]
	Sft->Trim, [0,300]
	Saf->Trim, [0,100]
	Smt->Trim, [0,25]
	Svl->Trim, [0,50]
	Sta->Trim, [0,20]

	PMI__Control, ev.top, /refresh
	return:PMI__Message, status
end

pro PMI__Button__Control__Jeong_dualinletonecomppixel, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 2)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__Jeong_dualinletonecomppixel, parent,value=value, separator=separator

	DualInletCompartment

	if n_elements(value) eq 0 then value = 'Fit dual input 2-compartment model (Pixel)'

	id = widget_button(parent $
	,	value 		= value	$
	,	event_pro 	= 'PMI__Button__Event__Jeong_dualinletonecomppixel'	$
	,	pro_set_value 	= 'PMI__Button__Control__Jeong_dualinletonecomppixel' $
	, 	separator 	= separator	)

	return, id

end
