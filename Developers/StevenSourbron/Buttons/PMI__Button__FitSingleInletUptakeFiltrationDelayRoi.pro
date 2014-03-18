pro PMI__Button__Event__FitSingleInletUptakeFiltrationDelayRoi, ev

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

	P = [0.1, 120.0/6000, 12/120.] ;[VP, FP, FE/FP]
	Fit = FitSingleInlet('2CUptakeFiltration', time, aif, curve, P, DELAY_PAR=Pd, DELAY_VALUES=[0,20,1], AKAIKE_ERROR=aic, /POSITIVITY, LIMITED_ABOVE=[1,0,1])

	PMI__Control, ev.top, Viewer = 'PMI__Display__PerfusionRoiOutput', Display=Display

	Display -> Set, /Refresh $
	,	Model = 'Two-compartment uptake filtration model with delay' $
	,	Time = Time $
	,	Curve = Curve $
	,	Fit = Fit $
	,	Units = Units $
	,	RoiName = Roi->Name() $
	,	Parameters = $
		[{Name:'Plasma Flow'				,Units:'ml/100ml/min'	,Value:6000D*P[1]			,Nr: 0,Rnd:1} $
		,{Name:'Plasma Mean Transit Time'	,Units:'sec'			,Value:1D*P[0]/P[1] 			,Nr: 1,Rnd:1} $
		,{Name:'Plasma Volume'				,Units:'ml/100ml'		,Value:100D*P[0]			,Nr: 2,Rnd:1} $
		,{Name:'Permeability-surface area product'		,Units:'%' 				,Value:100D*P[2]		,Nr: 5,Rnd:1} $
		,{Name:'Extraction Flow'			,Units:'ml/100ml/min'	,Value:6000D*P[1]*P[2]			,Nr: 6,Rnd:1} $
		,{Name:'Arterial Delay'						,Units:'sec'			,Value:1D*Pd 				,Nr: 8,Rnd:1} $
		,{Name:'Akaike Fit Error'			,Units:''				,Value:aic					,Nr:14,Rnd:1} ]

end

pro PMI__Button__Control__FitSingleInletUptakeFiltrationDelayRoi, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 1)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__FitSingleInletUptakeFiltrationDelayRoi, parent,value=value, separator=separator

	SingleInlet2CUptakeFiltration
	PMI__Display__PerfusionRoiOutput__Define

	if n_elements(value) eq 0 then value = 'Fit single-inlet uptake filtration model with delay (ROI)'

	id = widget_button(parent $
	,	value 		= value $
	,	event_pro 	= 'PMI__Button__Event__FitSingleInletUptakeFiltrationDelayRoi'$
	,	pro_set_value 	= 'PMI__Button__Control__FitSingleInletUptakeFiltrationDelayRoi' $
	, 	separator 	= separator	)

	return, id

end
