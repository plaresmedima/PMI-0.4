pro PMI__Button__Event__FitFermiRoi, ev

  PMI__Info, ev.top, stdy=Stdy

  if not TumorRoiAnalysis__input($
     ev.top $
     ,	Status	= status $
     ,	time 	= time $
     ,	curve 	= curve $
     ,	aif		= aif $
     ,	Units	= units $
     ,	Roi		= Roi $
     , 	ev=ev ) $
	then begin
		PMI__Message, Status
		return
     endif

	P = [0.015, 1.0, 0.5] ;[FP, a, b]
	Fit = FitSingleInlet('Fermi', time, aif, curve, P, DELAY_PAR=Pd, DELAY_VALUES=[0,20,1], AKAIKE_ERROR=aic, /noderivative)

	PMI__Control, ev.top, Viewer = 'PMI__Display__PerfusionRoiOutput', Display=Display

	Display -> Set, /Refresh $
	,	Model = 'Fermi model' $
	,	Time = Time $
	,	Curve = Curve $
	,	Fit = Fit $
	,	Units = Units $
	,	RoiName = Roi->Name() $
	,	Parameters = $
		[{Name:'Plasma Flow'		,Units:'ml/100ml/min'	,Value:6000D*P[0]	,Nr: 0,Rnd:1} $
		,{Name:'Exponent'			,Units:'1/sec'			,Value:1D*P[1]		,Nr: 1,Rnd:1} $
		,{Name:'Amplitude'			,Units:''				,Value:1D*P[2]		,Nr: 2,Rnd:1} $
		,{Name:'Delay'				,Units:'sec'			,Value:double(Pd)	,Nr: 4,Rnd:1} $
		,{Name:'Akaike Fit Error'	,Units:''				,Value:AIC			,Nr:14,Rnd:1} ]

end

pro PMI__Button__Control__FitFermiRoi, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 1)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__FitFermiRoi, parent,value=value, separator=separator

	SingleInletFermi
	PMI__Display__PerfusionRoiOutput__Define

	if n_elements(value) eq 0 then value = 'Fit Fermi model (ROI)'

	id = widget_button(parent 						$
	,	value 		= value		$
	,	event_pro 	= 'PMI__Button__Event__FitFermiRoi'	$
	,	pro_set_value 	= 'PMI__Button__Control__FitFermiRoi' $
	, 	separator 	= separator						)
	return, id
end
