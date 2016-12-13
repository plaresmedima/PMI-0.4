pro PMI__Button__Event__FitPatlakRoi, ev

	PMI__Info, ev.top, stdy=Stdy

	if not TumorRoiAnalysis__input($
		ev.top $
	,	status	= status $
	,	time 	= time $
	,	curve 	= curve $
	,	aif		= aif $
	,	Units	= units $
	,	Roi		= Roi $
	,	ev = ev ) $
	then begin
		PMI__Message, Status
		return
     endif

	P = [0.1, 12.0/6000] ;[VP, FE]
	Fit = FitSingleInlet('Patlak', time, aif, curve, P, AKAIKE_ERROR=aic, /POSITIVITY, LIMITED_ABOVE=[1,0])

	PMI__Control, ev.top, Viewer = 'PMI__Display__PerfusionRoiOutput', Display=Display

	Display -> Set, /Refresh $
	,	Model = 'Patlak model' $
	,	Time = Time $
	,	Curve = Curve $
	,	Fit = Fit $
	,	Units = Units $
	,	RoiName = Roi->Name() $
	,	Parameters = $
		[{Name:'Plasma Volume'		,Units:'ml/100ml'		,Value:100D*P[0]	,Nr: 1,Rnd:1} $
		,{Name:'Permeability-surface area product (PS)'	,Units:'ml/100ml/min'	,Value:6000D*P[1]	,Nr: 2,Rnd:1} $
		,{Name:'Akaike Fit Error'	,Units:''				,Value:AIC			,Nr:14,Rnd:1} ]

end

pro PMI__Button__Control__FitPatlakRoi, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 1)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__FitPatlakRoi, parent,value=value, separator=separator

	SingleInletPatlak
	PMI__Display__PerfusionRoiOutput__Define

	if n_elements(value) eq 0 then value = 'Fit non-linear Patlak model (ROI)'

	id = widget_button(parent 	$
	,	value 		= value		$
	,	event_pro 	= 'PMI__Button__Event__FitPatlakRoi'	$
	,	pro_set_value 	= 'PMI__Button__Control__FitPatlakRoi' $
	, 	separator 	= separator	)

	return, id

end
