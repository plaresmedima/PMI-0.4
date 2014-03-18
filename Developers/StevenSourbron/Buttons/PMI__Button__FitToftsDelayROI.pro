pro PMI__Button__Event__FitToftsDelayRoi, ev

	PMI__Info, ev.top, stdy=Stdy

	if not TumorRoiAnalysis__input($
		ev.top $
	,	status	= status $
	,	time 	= time $
	,	curve 	= curve $
	,	aif		= aif $
	,	Units	= units $
	,	Roi		= Roi $
	,	ev=ev $
	) then begin
		PMI__Message, Status
		return
     endif

	PMI__Message, status, 'Fitting..'

	P = [0.3, 120.0/6000] ;[VP+VE, FP]
	Fit = FitSingleInlet('Compartment', time, aif, curve, P, DELAY_PAR=Pd, DELAY_VALUES=[0,20,time[1]/2], AKAIKE_ERROR=aic, /POSITIVITY, LIMITED_ABOVE=[1,0])

	PMI__Control, ev.top, Viewer = 'PMI__Display__PerfusionRoiOutput', Display=Display

	Display -> Set, /Refresh $
	,	Model = 'Tofts model with delay' $
	,	Time = Time $
	,	Curve = Curve $
	,	Fit = Fit $
	,	Units = Units $
	,	RoiName = Roi->Name() $
	,	Parameters = $
		[{Name:'Ktrans'			,Units:'ml/100ml/min'	,Value:6000D*P[1]	,Nr: 0,Rnd:1} $
		,{Name:'ve'				,Units:'ml/100ml'		,Value:100D*P[0]	,Nr: 1,Rnd:1} $
		,{Name:'kep'			,Units:'1/sec'			,Value:1D*P[1]/P[0]	,Nr: 2,Rnd:1} $
		,{Name:'Arterial delay'	,Units:'sec'			,Value:double(Pd)	,Nr: 4,Rnd:1} $
		,{Name:'Akaike Fit Error',Units:''				,Value:aic			,Nr:14,Rnd:1} ]
end

pro PMI__Button__Control__FitToftsDelayRoi, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 1)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__FitToftsDelayRoi, parent,value=value, separator=separator

	SingleInletCompartment
	PMI__Display__PerfusionRoiOutput__Define

	if n_elements(value) eq 0 then value = 'ROI fit to single-inlet Tofts model with delay'

	id = widget_button(parent $
	,	value 		= value $
	,	event_pro 	= 'PMI__Button__Event__FitToftsDelayRoi'$
	,	pro_set_value 	= 'PMI__Button__Control__FitToftsDelayRoi' $
	, 	separator 	= separator)

	return, id

end
