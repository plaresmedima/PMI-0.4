function PMI__Button__Input__FitTwoCompartmentDelayRoi $
, 	ev $
,	Status	= id $
, 	Stdy	= Stdy $
,	time 	= time $
,	curve 	= roicurve $
,	aif		= aif $
,	Units	= units $
,	E12 	= E12 $
,	Roi		= Roi

	PMI__Info, ev.top, Status=id, Stdy=Stdy

   Series = Stdy->Names(0,ns,DefDim=3,ind=ind,sel=sel)
    Regions = Stdy->names(1,nr)

	Units = ['Signal Enhancement','Relative Signal Enhancement']

	in = cw_InputForm(/pos, ev=ev							$
	,	Title 	= 'Perfusion analysis setup' $
	,	Labels 	= 											$
		[	'Dynamic series' 								$
		,	'Arterial region'								$
		,	'Region of interest'							$
		,	'Approximate tracer concentrations by:'			$
		,	'Model constraint'								$
		,	'Length of baseline (# of dynamics)'			$
		,	'Patients hematocrit'							$
		]													$
	,	ListNames 		= [ Series,Regions,Regions,Units,'Filtration','Exchange']$
	,	ListNumbers 	= [	ns,nr,nr,2,2]$
	,	ListDefaults 	= [	sel,Stdy->sel(1),Stdy->sel(1),0,1]$
	,	DataDefaults 	= {	nb:10L,	hct:0.45E}$
	) & if size(in,/type) eq 1 then return, 0

	Series = Stdy->Obj(0,ind[in.select[0]])
	Art = Stdy->Obj(1,in.select[1])
	Roi = Stdy->Obj(1,in.select[2])
	Units = Units[in.select[3]]
	E12 = float(in.select[4])


    Time = Series->c(1)
    Time = Time-Time[0]


	; GET AIF


	Aif = PMI__RoiCurve(Stdy->DataPath(), Series, Art, id, cnt=cnt)
	if cnt eq 0 then return, 0
	Aif = LMU__Enhancement(Aif,in.data.nb,relative=units eq 'Relative Signal Enhancement')
	Aif = Aif/(1-in.data.hct)


	; GET ROICURVE


	RoiCurve = PMI__RoiCurve(Stdy->DataPath(), Series, Roi, id, cnt=cnt)
	if cnt eq 0 then return, 0
	RoiCurve = LMU__Enhancement(RoiCurve,in.data.nb,relative = units eq 'Relative Signal Enhancement')

	return, 1
end

pro PMI__Button__Event__FitTwoCompartmentDelayRoi, ev

  if not PMI__Button__Input__FitTwoCompartmentDelayRoi($
     ev $
     ,	Status	= status $
     , 	Stdy	= Stdy $
     ,	time 	= time $
     ,	curve 	= curve $
     ,	aif		= aif $
     ,	Units	= units $
     ,	E12 	= E12 $
     ,	Roi		= Roi ) $
	then begin
		PMI__Message, Status
		return
     endif

  	PMI__Message, Status, 'Fitting..'

  	Fit = TwoCompartmentDelayFit_Indirect(Time,Curve,Aif,E12,Init=5,Delay=Pd,Kinetic=Pk,Convective=Pc,AIC=AIC,/mp,/noderivative,/quiet)

	PMI__Control, ev.top, Viewer = 'PMI__Display__PerfusionRoiOutput', Display=Display

	Display -> Set, /Refresh $
	,	Model = 'Two-compartment model with delay' $
	,	Time = Time $
	,	Curve = Curve $
	,	Fit = Fit $
	,	Units = Units $
	,	RoiName = Roi->Name() $
	,	Parameters = $
		[{Name:'Plasma Flow'					,Units:'ml/100ml/min'	,Value:6000D*Pc[4]			,Nr: 0,Rnd:1} $
		,{Name:'Plasma Mean Transit Time'		,Units:'sec'			,Value:1D*Pk[2] 			,Nr: 1,Rnd:1} $
		,{Name:'Plasma Volume'					,Units:'ml/100ml'		,Value:100.0*Pc[2]			,Nr: 2,Rnd:1} $
		,{Name:'Interstitial Mean Transit Time'	,Units:'sec'			,Value:1D*Pk[3]				,Nr: 3,Rnd:1} $
		,{Name:'Interstitial Volume'			,Units:'ml/100ml'		,Value:100D*Pc[3]			,Nr: 4,Rnd:1} $
		,{Name:'Extraction Fraction'			,Units:'%' 				,Value:100D*Pc[0]/Pc[4]		,Nr: 5,Rnd:1} $
		,{Name:'Extraction Flow'				,Units:'ml/100ml/min'	,Value:6000D*Pc[0]			,Nr: 6,Rnd:1} $
		,{Name:'Delay'							,Units:'sec'			,Value:1D*Pd				,Nr: 8,Rnd:1} $
		,{Name:'Akaike Fit Error'				,Units:''				,Value:AIC					,Nr:14,Rnd:1} ]

end


pro PMI__Button__Control__FitTwoCompartmentDelayRoi, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 1)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end



function PMI__Button__FitTwoCompartmentDelayRoi, parent,value=value, separator=separator

	if n_elements(value) eq 0 then value = 'Fit 2-compartment model with delay (ROI)'

	id = widget_button(parent $
	,	value 		= value	$
	,	event_pro 	= 'PMI__Button__Event__FitTwoCompartmentDelayRoi' $
	,	pro_set_value 	= 'PMI__Button__Control__FitTwoCompartmentDelayRoi' $
	, 	separator 	= separator	)

	return, id
end
