function PMI__Button__Input__TMUH_USPIO_DeconvolutionAnalysisRoi $
, 	top $
,	Status	 = id $
,	time	 = time $
,	curve 	 = roicurve $
,	aif 	 = aif $
,	ROI 	 = roi $
,	nb=nb $
,	ev = ev

	PMI__Info, top, Status=id, Stdy=Stdy
	PMI__Message, id, 'Getting input..'

   	Series = Stdy->Names(0,DefDim=3,ind=ind,sel=sel)

	in = PMI__Form(ev.top, Title='Perfusion analysis setup', [$
		ptr_new({Type:'DROPLIST',Tag:'ser', Label:'Dynamic series', Value:Series, Select:sel}), $
		ptr_new({Type:'DROPLIST',Tag:'aif', Label:'Arterial Region', Value:Stdy->names(1), Select:Stdy->sel(1)}), $
		ptr_new({Type:'DROPLIST',Tag:'roi', Label:'Tissue Region', Value:Stdy->names(1), Select:Stdy->sel(1)}), $
		ptr_new({Type:'VALUE'	,Tag:'nb' , Label:'Length of baseline (# of dynamics)', Value:5B})])
		IF in.cancel THEN return, 0

    Series = Stdy->Obj(0,ind[in.ser])
    Art = Stdy->Obj(1,in.aif)
    Roi = Stdy->Obj(1,in.roi)
    nb = in.nb

    Time = Series->c(1)
    Time = Time-Time[0]

    Aif = PMI__RoiCurve(Stdy->DataPath(), Series, Art, id, cnt=cnt)
    if cnt eq 0 then begin
    	ok = dialog_message(/information,'Region <' + Art->Name() + '> is not defined on Series <' + Series->Name() + '>')
    	return, 0
    endif
	RoiCurve = PMI__RoiCurve(Stdy->DataPath(), Series, Roi, id, cnt=cnt)
    if cnt eq 0 then begin
    	ok = dialog_message(/information,'Region <' + Roi->Name() + '> is not defined on Series <' + Series->Name() + '>')
    	return, 0
    endif

	Aif = LMU__Enhancement(Aif,nb,relative=2)
	RoiCurve = LMU__Enhancement(RoiCurve,nb,relative=2)

	return, 1

end

pro PMI__Button__Event__TMUH_USPIO_DeconvolutionAnalysisRoi, ev

	PMI__Info, ev.top, stdy=Stdy, Status=status

	PMI__Message, status, 'Getting input..'

  	if not PMI__Button__Input__TMUH_USPIO_DeconvolutionAnalysisRoi(ev.top $
     ,	time 	= time $
     ,	curve 	= roicurve $
     ,	aif		= aif $
     ,	Roi		= Roi $
     ,	ev		= ev $
     ) then begin
     	PMI__Message, Status
     	return
     endif

	PMI__Message, status, 'Deconvolving..'

	IRF = DeconvolveCurve(time,	roicurve, aif, dt=dt, CurveRegr=c, Fit=Fit, $
		pc='GCV', wm=1L, m0=0.001, m1=1.0, nm=100L, Quad='O2')

	PMI__Control, ev.top, Viewer = 'PMI__Display__PerfusionRoiOutput', Display=Display

	Display -> Set, /Refresh $
	,	Model = 'Deconvolution (Residue Detection)' $
	,	Time = dt*findgen(n_elements(c)) $
	,	Curve = c $
	,	Fit = Fit $
	,	Units = 'Relative Signal Enhancement' $
	,	RoiName = Roi->Name() $
	,	Parameters = $
		[ {Name:'Blood Flow'			,Units:'ml/100ml/min'	,Value:6000.0*max(IRF)			,Nr:0,Rnd:1} $
		, {Name:'Mean Transit Time'		,Units:'sec'			,Value:dt*total(IRF)/max(IRF)	,Nr:1,Rnd:1} $
		, {Name:'Blood Volume'	,Units:'ml/100ml'		,Value:100.0*dt*total(IRF) 		,Nr:2,Rnd:1} ]

end



pro PMI__Button__Control__TMUH_USPIO_DeconvolutionAnalysisRoi, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->names(1,nr)
		sensitive = (ns gt 0) and (nr gt 1)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__TMUH_USPIO_DeconvolutionAnalysisRoi, parent,value=value,separator=separator

	PMI__Display__PerfusionRoiOutput__Define

	if n_elements(value) eq 0 then value = 'Deconvolution analysis (ROI) - Residue detection'

    id = widget_button(parent $
    , 	value 		= value $
	, 	event_pro	= 'PMI__Button__Event__TMUH_USPIO_DeconvolutionAnalysisRoi'	$
	,	pro_set_value 	= 'PMI__Button__Control__TMUH_USPIO_DeconvolutionAnalysisRoi' $
	, 	separator 	= separator	)

	return, id
end

