function PMI__Button__Input__FitPatlakLinearRoi, top $
, 	Status	 = status $
,	time	 = time $
,	curve 	 = curve $
,	aif 	 = aif $
,	RoiName  = RoiName $
,	nb		 = nb $
,	n0		 = n0

	PMI__Info, top, Status=status, Stdy=Stdy
	PMI__Message, status, 'Getting input..'

   	Series = Stdy->Names(0,DefDim=3,ind=ind,sel=sel)
    Units = ['Signal Enhancement (T1)','Relative Signal Enhancement (T1)','Relative Signal Enhancement (T2)']

	in = PMI__Form(top, Title='Perfusion analysis setup', [$
		ptr_new({Type:'DROPLIST',Tag:'ser', Label:'Dynamic series', Value:Series, Select:sel}), $
		ptr_new({Type:'DROPLIST',Tag:'aif', Label:'Arterial Region', Value:Stdy->names(1), Select:Stdy->sel(1)}), $
		ptr_new({Type:'DROPLIST',Tag:'roi', Label:'Tissue Region', Value:Stdy->names(1), Select:Stdy->sel(1)}), $
		ptr_new({Type:'DROPLIST',Tag:'rel', Label:'Approximate tracer concentrations by:', Value:Units, Select:1}), $
		ptr_new({Type:'VALUE'	,Tag:'nb' , Label:'Length of baseline (nr. of dynamics)', Value:1L}),$
		ptr_new({Type:'VALUE'	,Tag:'n0' , Label:'Nr. of initial dynamics to ignore', Value:1L}),$
		ptr_new({Type:'VALUE'	,Tag:'hct', Label:'Patients hematocrit', Value:0.45})])
		IF in.cancel THEN return, 0

    Series = Stdy->Obj(0,ind[in.ser])
    Art = Stdy->Obj(1,in.aif)
    Roi = Stdy->Obj(1,in.roi)
    RoiName = Roi->Name()
    nb = in.nb
    n0 = in.n0

    Time = Series->c(1)
    Time = Time-Time[0]

    Aif = PMI__RoiCurve(Stdy->DataPath(), Series, Art, status, cnt=cnt)
    if cnt eq 0 then begin
    	ok = dialog_message(/information,'Region <' + Art->Name() + '> is not defined on Series <' + Series->Name() + '>')
    	return, 0
    endif
	Curve = PMI__RoiCurve(Stdy->DataPath(), Series, Roi, status, cnt=cnt)
    if cnt eq 0 then begin
    	ok = dialog_message(/information,'Region <' + Roi->Name() + '> is not defined on Series <' + Series->Name() + '>')
    	return, 0
    endif

	Aif = LMU__Enhancement(Aif,nb,relative=in.rel)/(1-in.hct)
	Curve = LMU__Enhancement(Curve,nb,relative=in.rel)

	return, 1
end


pro PMI__Button__Event__FitPatlakLinearRoi, ev

	if not PMI__Button__Input__FitPatlakLinearRoi(ev.top $
	,	status 	= status $
	,	time 	= time $
	,	curve 	= curve $
	,	aif		= aif $
	,	RoiName	= RoiName $
	,	nb		= nb $
	,	n0		= n0 ) $
	then begin
		PMI__Message, Status
		return
    endif

	Fit = FitPatlak(time, curve, aif, Interval=[time[n0],max(time)], Pars=P, PatlakX=X, PatlakY=Y)

	PMI__Control, ev.top, Viewer = 'PMI__Display__PerfusionRoiOutput', Display=Display

	Display -> Set, /Refresh $
	,	Style = 1B $
	,	Model = 'Linearized Patlak model' $
	,	Time = X $
	,	Curve = Y $
	,	Fit = P[0] + P[1]*X $
	,	Units = 'Dimensionless' $
	,	RoiName = RoiName $
	,	Parameters = $
		[{Name:'Plasma Volume'		,Units:'ml/100ml'		,Value:100D*P[0]	,Nr: 1,Rnd:1} $
		,{Name:'Extraction Flow'	,Units:'ml/100ml/min'	,Value:6000D*P[1]	,Nr: 2,Rnd:1} ]

end

pro PMI__Button__Control__FitPatlakLinearRoi, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 1)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__FitPatlakLinearRoi, parent,value=value, separator=separator

	PMI__Display__PerfusionRoiOutput__Define

	if n_elements(value) eq 0 then value = 'Fit Linearized Patlak (ROI)'

	id = widget_button(parent $
	,	value = value $
	,	event_pro = 'PMI__Button__Event__FitPatlakLinearRoi' $
	,	pro_set_value = 'PMI__Button__Control__FitPatlakLinearRoi' $
	, 	separator = separator	)

	return, id
end
