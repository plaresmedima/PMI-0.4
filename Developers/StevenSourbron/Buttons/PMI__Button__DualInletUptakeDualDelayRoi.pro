pro PMI__Button__Event__DualInletUptakeDualDelayRoi, ev

	if not FitLiverRoi__input($
		ev.top $
	, 	Stdy	= Stdy $
	,	status 	= status $
	,	time 	= time $
	,	curve 	= curve $
	,	aif		= aif $
	,	vif 	= vif $
	,	Units	= units $
	,	RoiName	= RoiName $
	,	ev		= ev $
	) then begin
		PMI__Message, Status
		return
     endif

	PMI__Message, status, 'Fitting..'

	P = [0.2*50/6000D,0.8*50/6000.,20.,0.2]
	Fit = FitDualInletDualDelay('FitDualInletUptake',time,curve,aif,vif,Pars=P,Delay=Pd,AIC=AIC,/noderivative,/quiet,/constrained)

	PMI__Control, ev.top, Viewer='PMI__Display__PerfusionRoiOutput', Display=Display

	Display -> Set, /Refresh $
	,	Model = 'Dual-inlet 2-compartment uptake model with dual delay' $
	,	Time = Time $
	,	Curve = Curve $
	,	Fit = Fit $
	,	Units = Units $
	,	RoiName = RoiName $
	,	Parameters = $
		[{Name:'Arterial Plasma Flow'	,Units:'ml/100ml/min'	,Value:6000*P[0]						,Nr: 0,Rnd:1} $
		,{Name:'Venous Plasma Flow'		,Units:'ml/100ml/min'	,Value:6000*P[1]						,Nr: 1,Rnd:1} $
		,{Name:'Arterial Flow Fraction'	,Units:'%' 				,Value:100*P[0]/(P[0]+P[1])				,Nr: 2,Rnd:1} $
		,{Name:'Mean Transit Time'		,Units:'sec'			,Value:P[2]								,Nr: 3,Rnd:1} $
		,{Name:'Extracellular Volume'	,Units:'ml/100ml'		,Value:100*(P[0]+P[1])*P[2]/(1-P[3])	,Nr: 4,Rnd:1} $
		,{Name:'Uptake Fraction'		,Units:'%' 				,Value:100*P[3]							,Nr: 9,Rnd:1} $
		,{Name:'Uptake Flow'			,Units:'ml/100ml/min'	,Value:6000*(P[0]+P[1])*P[3]/(1-P[3])	,Nr:10,Rnd:1} $
		,{Name:'Arterial Delay'			,Units:'sec'			,Value:double(Pd[0])					,Nr:11,Rnd:1} $
		,{Name:'Venous Delay'			,Units:'sec'			,Value:double(Pd[1])					,Nr:12,Rnd:1} $
		,{Name:'Akaike Fit Error'		,Units:''				,Value:AIC								,Nr:14,Rnd:1} ]

end

pro PMI__Button__Control__DualInletUptakeDualDelayRoi, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 2)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__DualInletUptakeDualDelayRoi, parent,value=value, separator=separator

	ok=FitDualInletUptake()
	PMI__Display__PerfusionRoiOutput__Define

	if n_elements(value) eq 0 then value = 'Fit dual-inlet uptake model with dual delay (ROI)'

	id = widget_button(parent $
	,	value 		= value $
	,	event_pro 	= 'PMI__Button__Event__DualInletUptakeDualDelayRoi'$
	,	pro_set_value 	= 'PMI__Button__Control__DualInletUptakeDualDelayRoi' $
	, 	separator 	= separator	)

	return, id

end
