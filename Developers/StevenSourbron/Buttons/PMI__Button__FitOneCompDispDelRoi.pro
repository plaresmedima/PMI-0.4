pro PMI__Button__Event__FitOneCompDispDelRoi, ev

	PMI__Info, ev.top, Stdy=Stdy

	if not TumorRoiAnalysis__input($
		ev.top $
	,	status	= status $
	,	time 	= time $
	,	curve 	= curve $
	,	aif		= aif $
	,	Units	= units $
	,	Roi		= Roi $
	,	ev=ev ) $
	then begin
		PMI__Message, Status
		return
     endif

	PMI__Message, status, 'Fitting..'

	Fit = FitOneCompDispDel(time,curve,aif,init=5,Delay=Pd,AIC=AIC,Pars=P,/mp,/noderivative,/quiet)

	PMI__Control, ev.top, Viewer = 'PMI__Display__PerfusionRoiOutput', Display=Display

	Display -> Set, /Refresh $
	,	Model = '1-compartment model with dispersion and delay' $
	,	Time = Time $
	,	Curve = Curve $
	,	Fit = Fit $
	,	Units = Units $
	,	RoiName = Roi->Name() $
	,	Parameters = $
		[{Name:'Plasma Flow'				,Units:'ml/100ml/min'	,Value:6000.0*P[0]	,Nr: 0,Rnd:1} $
		,{Name:'Mean Transit Time'			,Units:'sec'			,Value:1D*P[1]/P[0] ,Nr: 1,Rnd:1} $
		,{Name:'Volume of Distribution'		,Units:'ml/100ml'		,Value:100.0*P[1]	,Nr: 2,Rnd:1} $
		,{Name:'Arterial Dispersion Time'	,Units:'sec'			,Value:1D*P[2]		,Nr: 4,Rnd:1} $
		,{Name:'Arterial Delay Time'		,Units:'sec'			,Value:1D*Pd		,Nr: 5,Rnd:1} $
		,{Name:'Akaike Fit Error'			,Units:''				,Value:AIC			,Nr:14,Rnd:1} ]

end

pro PMI__Button__Control__FitOneCompDispDelRoi, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 1)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__FitOneCompDispDelRoi, parent,value=value, separator=separator

	PMI__Display__PerfusionRoiOutput__Define

	if n_elements(value) eq 0 then value = 'ROI fit to single-inlet 1-compartment model with delay and dispersion'

	id = widget_button(parent $
	,	value 		= value $
	,	event_pro 	= 'PMI__Button__Event__FitOneCompDispDelRoi'	$
	,	pro_set_value 	= 'PMI__Button__Control__FitOneCompDispDelRoi' $
	, 	separator 	= separator	)

	return, id

end
