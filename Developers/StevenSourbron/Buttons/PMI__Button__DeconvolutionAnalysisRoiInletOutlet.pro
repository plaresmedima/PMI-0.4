function PMI__Button__DeconvolutionAnalysisRoiInletOutlet__Options, options, ev=ev

	pc = ['LCC','GCV']

	in = cw_InputForm(/pos, ev=ev $
	,	Title = 'Deconvolution analysis setup: advanced options' $
	,	Labels = $
		[	'Parameter-choice method' $
		,	'Minimal regularization parameter' $
		,	'Maximal regularization parameter' $
		,	'Number of regularization parameters' $
		,	'Parameter smoothing window' $
		]$
	,	ListNames = pc $
	,	DataDefaults = {m0:0.001, m1:1.0, nm:100L, wm:1L}$
	) & if size(in,/type) eq 1 then return, 0

	options = {$
		pc:pc[in.select[0]] $
	, 	m0:in.data.m0 $
	, 	m1:in.data.m1 $
	, 	nm:in.data.nm $
	, 	wm:in.data.wm }

	return, 1
end


pro PMI__Button__Event__DeconvolutionAnalysisRoiInletOutlet, ev

	PMI__Info, ev.top, stdy=Stdy

  	if not TumorRoiAnalysis__input($
     ev.top $
     ,	Status	= status $
     ,	time 	= time $
     ,	curve 	= roicurve $
     ,	aif		= aif $
     ,	Units	= units $
     ,	Roi		= Roi $
     ,	ev = ev $
     ) then begin
     	PMI__Message, Status
     	return
     endif

	if not PMI__Button__DeconvolutionAnalysisRoiInletOutlet__Options(opt,ev=ev) then begin
     	PMI__Message, Status
     	return
     endif

	PMI__Message, status, 	'Deconvolving..'

	IRF = DeconvolveCurve(time $
	,	roicurve $
	,	aif $
	,	dt=dt $
	,	pc=opt.pc, wm=opt.wm $
	, 	m0=opt.m0, m1=opt.m1, nm=opt.nm $
	,	CurveRegr=c,Fit=Fit)

	Time = dt*dindgen(n_elements(c))

	PMI__Control, ev.top, Viewer = 'PMI__Display__PerfusionRoiOutput'
	PMI__Info, ev.top, Display=Display

	Display -> Set, /Refresh $
	,	Model = 'Deconvolution (Outlet Detection)' $
	,	Time = Time $
	,	Curve = c $
	,	Fit = Fit $
	,	Units = Units $
	,	RoiName = Roi->Name() $
	,	Parameters = $
		[{Name:'Mean Transit Time'	,Units:'sec'		,Value:total(Time*IRF)/total(IRF)	,Nr:1,Rnd:1} $
		,{Name:'Plasma Volume'		,Units:'ml/100ml'	,Value:100D*dt*total(IRF) 			,Nr:2,Rnd:1} ]

end

pro PMI__Button__Control__DeconvolutionAnalysisRoiInletOutlet, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->names(1,nr)
		sensitive = (ns gt 0) and (nr gt 1)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end


function PMI__Button__DeconvolutionAnalysisRoiInletOutlet, parent,value=value,separator=separator

	PMI__Display__PerfusionRoiOutput__Define

	if n_elements(value) eq 0 then value = 'Deconvolution analysis (ROI) - Inlet/Outlet detection'

    id = widget_button(parent $
    , 	value 		= value	$
	, 	event_pro	= 'PMI__Button__Event__DeconvolutionAnalysisRoiInletOutlet'	$
	,	pro_set_value 	= 'PMI__Button__Control__DeconvolutionAnalysisRoiInletOutlet' $
	, 	separator 	= separator	)

	return, id
end

