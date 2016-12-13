pro PMI__Button__Event__FitModifiedToftsDelayROI, ev

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


	PMI__Message, Status, 'Fitting..'

	P = [0.3, 2.0/3, 12.0/6000] 	;[VP+VE, VE/(VP+VE), FE]
	Fit = FitSingleInlet('ModifiedTofts', time, aif, curve, P, DELAY_PAR=Pd, DELAY_VALUES=[0,20,time[1]/2], AKAIKE_ERROR=aic, /POSITIVITY, LIMITED_ABOVE=[1,1,0])

	PMI__Control, ev.top, Viewer = 'PMI__Display__PerfusionRoiOutput', Display=Display

	Display -> Set, /Refresh $
	,	Model = 'Modified Tofts model with delay' $
	,	Time = Time $
	,	Curve = Curve $
	,	Fit = Fit $
	,	Units = Units $
	,	RoiName = Roi->Name() $
	,	Parameters = $
		[{Name:'Plasma Volume',Units:'ml/100ml',Value:100D*P[0]*(1-P[1]),Nr: 0,Rnd:1} $
		,{Name:'Ktrans',Units:'ml/100ml/min',Value:6000D*P[2],Nr: 3,Rnd:1} $
		,{Name:'EES Volume',Units:'ml/100ml',Value:100D*P[0]*P[1],Nr: 2,Rnd:1} $
		,{Name:'EES Mean Transit Time',Units:'sec',Value:1D*P[0]*P[1]/P[2],Nr: 1,Rnd:1} $
		,{Name:'Arterial Delay',Units:'sec',Value:Double(Pd),Nr: 4,Rnd:1} $
		,{Name:'Akaike Fit Error',Units:'',Value:AIC,Nr:14,Rnd:1} ]
end

pro PMI__Button__Control__FitModifiedToftsDelayROI, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 1)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__FitModifiedToftsDelayROI, parent,value=value, separator=separator

	SingleInletModifiedTofts
	PMI__Display__PerfusionRoiOutput__Define

	if n_elements(value) eq 0 then value = 'Fit Modified Tofts model with delay (ROI)'

	id = widget_button(parent $
	,	value = value	$
	,	event_pro = 'PMI__Button__Event__FitModifiedToftsDelayROI'$
	,	pro_set_value = 'PMI__Button__Control__FitModifiedToftsDelayROI' $
	, 	separator = separator	)

	return, id

end
