pro PMI__Button__Event__FitSingleInletUptakeDelayRoi, ev

	PMI__Info, ev.top, Stdy=Stdy

	if not TumorRoiAnalysis__input($
		ev.top $
	,	status	= status $
	,	time 	= time $
	,	curve 	= curve $
	,	aif		= aif $
	,	Units	= units $
	,	Roi		= Roi $
	,	ev		= ev ) $
	then begin
		PMI__Message, Status
		return
    endif

	Fit = FitSingleInletUptakeDelay(Time,Curve,Aif,Pars=Pk,Init=5,Delay=Pd,AIC=AIC,Convective=Pc,/mp,/noderivative,/quiet)

	PMI__Control, ev.top, Viewer = 'PMI__Display__PerfusionRoiOutput', Display=Display

	Display -> Set, /Refresh $
	,	Model = 'Two-compartment uptake model with delay' $
	,	Time = Time $
	,	Curve = Curve $
	,	Fit = Fit $
	,	Units = Units $
	,	RoiName = Roi->Name() $
	,	Parameters = $
		[{Name:'Plasma Flow'				,Units:'ml/100ml/min'	,Value:6000D*Pc[2]			,Nr: 0,Rnd:1} $
		,{Name:'Plasma Mean Transit Time'	,Units:'sec'			,Value:1D*Pk[1] 			,Nr: 1,Rnd:1} $
		,{Name:'Plasma Volume'				,Units:'ml/100ml'		,Value:100D*Pc[1]			,Nr: 2,Rnd:1} $
		,{Name:'Extraction Fraction'		,Units:'%' 				,Value:100D*Pc[0]/Pc[2]		,Nr: 5,Rnd:1} $
		,{Name:'Extraction Flow'			,Units:'ml/100ml/min'	,Value:6000D*Pc[0]			,Nr: 6,Rnd:1} $
		,{Name:'Delay'						,Units:'sec'			,Value:1D*Pd 				,Nr: 8,Rnd:1} $
		,{Name:'Akaike Fit Error'			,Units:''				,Value:AIC					,Nr:14,Rnd:1} ]

end

pro PMI__Button__Control__FitSingleInletUptakeDelayRoi, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 1)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__FitSingleInletUptakeDelayRoi, parent,value=value, separator=separator

	if n_elements(value) eq 0 then value = 'Fit single-inlet uptake model with delay (ROI)'

	id = widget_button(parent $
	,	value 		= value $
	,	event_pro 	= 'PMI__Button__Event__FitSingleInletUptakeDelayRoi'$
	,	pro_set_value 	= 'PMI__Button__Control__FitSingleInletUptakeDelayRoi' $
	, 	separator 	= separator	)

	return, id

end
