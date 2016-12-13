function PMI__Button__Input__FitPlugFlowDispOutlet $
,	ev $
,	Stdy	= Stdy $
,	status 	= status $
,	time 	= time $
,	curve 	= roicurve $
,	aif		= aif $
,	Units 	= Units $
,	RoiName	= RoiName $
,	Delay	= Delay

	PMI__Info, ev.top, Status=status, Stdy=Stdy, Series=Series

	Series = Stdy->Names(0,ns,DefDim=3,ind=ind,sel=sel)
	Regions = Stdy->names(1,nr)

	Units = ['Signal Enhancement','Relative Signal Enhancement']

	in = cw_InputForm(/pos, ev=ev $
	,	Title 	= 'Fit plug flow model' $
	,	Labels 	= $
		[	'Dynamic series' $
		,	'Arterial region' $
		,	'Region of interest'$
		,	'Approximate tracer concentrations by:'	$
		,	'Length of baseline (# of dynamics)'$
		,	'Patients hematocrit'$
		,	'Maximal Transit Time (sec)' $
		,	'Transit Time Resolution (sec)' $
		]$
	,	ListNames = [ Series,Regions,Regions,Units ]$
	,	ListNumbers 	= [	ns,nr,nr,2]$
	,	ListDefaults 	= [	sel,Stdy->sel(1),Stdy->sel(1),0]$
	,	DataDefaults 	= {	nb:10L, hct:0.45, Tmax:10E, Tres:1.0}$
	) & if size(in,/type) eq 1 then return, 0

	Series = Stdy->Obj(0,ind[in.select[0]])
	Art 	= Stdy->Obj(1,in.select[1])
	Roi 	= Stdy->Obj(1,in.select[2])
	Units 	= Units[in.select[3]]

	Delay = in.data.Tres*findgen(1+floor(in.data.Tmax/in.data.Tres))

	Time = Series->c(1)
	Time = Time-Time[0]

	RoiName = Roi->Name()

	Aif 		= PMI__RoiCurve(Stdy->DataPath(),Series,Art,status,cnt=cnt) & if cnt eq 0 then return, 0
	RoiCurve 	= PMI__RoiCurve(Stdy->DataPath(),Series,Roi,status,cnt=cnt) & if cnt eq 0 then return, 0

	Aif 		= LMU__Enhancement(Aif		,in.data.nb,relative=in.select[3] eq 1)/(1-in.data.hct)
	RoiCurve 	= LMU__Enhancement(RoiCurve	,in.data.nb,relative=in.select[3] eq 1)

	return, 1
end

pro PMI__Button__Event__FitPlugFlowDispOutlet, ev

	PMI__Info, ev.top, Stdy=Stdy

	if not PMI__Button__Input__FitPlugFlowDispOutlet($
		ev $
	,	Stdy	= Stdy $
	,	status 	= status $
	,	time 	= time $
	,	curve 	= curve $
	,	aif		= aif $
	,	Units 	= Units $
	,	RoiName	= RoiName $
	,	Delay	= Pd ) $
	then begin
		PMI__Message, Status
		return
    endif

	PMI__Message, status, 'Fitting..'

	Pars = UpslopePerfusion(time,curve,aif)
	Fit = FitKetyDel(time,curve,aif,init=5,Delay=Pd,Pars=P,AIC=AIC,/mp,/noderivative,/quiet)


	PMI__Control, ev.top, Viewer = 'PMI__Display__PerfusionRoiOutput', Display=Display

	Display -> Set, /Refresh $
	,	Model = 'Plug flow - one compartment (outlet detection)' $
	,	Time = Time $
	,	Curve = Curve $
	,	Fit = Fit $
	,	Units = Units $
	,	RoiName = RoiName $
	,	Parameters = $
		[{Name:'Plasma Volume'		,Units:'ml/100ml'	,Value:100D*P[1]	,Nr: 1,Rnd:1} $
		,{Name:'Dispersion Time'	,Units:'sec'		,Value:P[1]/P[0]	,Nr: 2,Rnd:1} $
		,{Name:'Delay'				,Units:'sec'		,Value:1D*Pd		,Nr: 3,Rnd:1} $
		,{Name:'Akaike Fit Error'	,Units:''			,Value:AIC			,Nr:14,Rnd:1} ]

end

pro PMI__Button__Control__FitPlugFlowDispOutlet, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 1)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__FitPlugFlowDispOutlet, parent,value=value, separator=separator

	PMI__Display__PerfusionRoiOutput__Define

	if n_elements(value) eq 0 then value = 'ROI fit to plug flow-dispersion outlet model'

	id = widget_button(parent $
	,	value 		= value	$
	,	event_pro 	= 'PMI__Button__Event__FitPlugFlowDispOutlet'$
	,	pro_set_value 	= 'PMI__Button__Control__FitPlugFlowDispOutlet' $
	, 	separator 	= separator	)

	return, id

end
