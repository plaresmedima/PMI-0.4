function PMI__Button__Input__CardiacFermiRoi $
   ,    ev $
   ,    Stdy = Stdy $
   ,    status = status $
   ,    time = time $
   ,    aif = aif $
   ,	curve = curve $
   ,	Roi =Roi

	;GET USER INPUT

    PMI__Info, ev.top, Status=Status, Stdy=Stdy

    Series = Stdy->Names(0,DefDim=3,ind=ind,sel=sel)

	in = PMI__Form(ev.top, Title='Perfusion analysis setup', [$
	ptr_new({Type:'DROPLIST',Tag:'ser', Label:'Dynamic series', Value:Series, Select:sel}), $
	ptr_new({Type:'DROPLIST',Tag:'aif', Label:'Arterial Region', Value:Stdy->names(1), Select:stdy->sel(1)}), $
	ptr_new({Type:'DROPLIST',Tag:'roi', Label:'Tissue Region', Value:Stdy->names(1), Select:stdy->sel(1)}), $
	ptr_new({Type:'DROPLIST',Tag:'rel', Label:'Inhomogeneity correction?', Value:['No','Yes'], Select:0}), $
	ptr_new({Type:'VALUE'	,Tag:'nb' , Label:'Length of baseline (# of dynamics)', Value:2})])
	IF in.cancel THEN return, 0

    Series = Stdy->Obj(0,ind[in.ser])
    Art = Stdy->Obj(1,in.aif)
    Roi = Stdy->Obj(1,in.roi)

    Time = Series->c(1)
    Time = Time-Time[0]

	;GET ROI CURVES

    Aif = PMI__RoiCurve(Stdy->DataPath(), Series, Art, status, cnt=cnt, slice=AIFslice)
    if cnt eq 0 then begin
    	ok = dialog_message(/information,'Arterial region is empty')
    	return, 0
    end
    Aif = LMU__Enhancement(Aif,in.nb,relative=in.rel)/0.55

	Curve = PMI__RoiCurve(Stdy->DataPath(), Series, Roi, status, cnt=cnt, slice=ROIslice)
    if cnt eq 0 then begin
    	ok = dialog_message(/information,'Region <' + Roi->Name() + '> is not defined on Series <' + Series->Name() + '>')
    	return, 0
    endif
    Curve = LMU__Enhancement(Curve,in.nb,relative=in.rel)

  	return, 1
end

pro PMI__Button__Event__CardiacFermiRoi, ev

  PMI__Info, ev.top, stdy=Stdy

    if not PMI__Button__Input__CardiacFermiRoi(ev $
    ,  Stdy = Stdy $
    ,  status = status $
    ,  time = time $
    ,  aif = aif $
    ,  curve = curve $
    ,  Roi = Roi $
    )  then begin
     	PMI__Message, Status
     	return
     endif

	plot, time, aif, title='Arterial Input Function', xtitle='time (sec)', ytitle='Concentration (mM)'

	PMI__Message, status, 'Fitting Fermi model...'

	P = [0.015, 1.0, 0.5] ;[FP, a, b]
	Fit = FitSingleInlet('Fermi', time, aif, curve, P, DELAY_PAR=Pd, DELAY_VALUES=[0,5,0.25], /noderivative)

	PMI__Control, ev.top, Viewer = 'PMI__Display__PerfusionRoiOutput', Display=Display

	Display -> Set, /Refresh $
	,	Model = 'Fermi model' $
	,	Time = Time $
	,	Curve = Curve $
	,	Fit = Fit $
	,	Units = Units $
	,	RoiName = Roi->Name() $
	,	Parameters = $
		[{Name:'Blood Flow'		,Units:'ml/100ml/min'	,Value:6000D*P[0]/0.55	,Nr: 0,Rnd:1} $
		,{Name:'Exponent (a)'	,Units:'1/sec'			,Value:1D*P[1]			,Nr: 1,Rnd:1} $
		,{Name:'Amplitude (b)'	,Units:''				,Value:1D*P[2]			,Nr: 2,Rnd:1} $
		,{Name:'Delay'			,Units:'sec'			,Value:double(Pd)		,Nr: 4,Rnd:1} ]

end

pro PMI__Button__Control__CardiacFermiRoi, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 1)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__CardiacFermiRoi, parent,value=value, separator=separator

	SingleInletFermi
	PMI__Display__PerfusionRoiOutput__Define

	if n_elements(value) eq 0 then value = 'Fit Fermi model (ROI)'

	id = widget_button(parent 						$
	,	value 		= value		$
	,	event_pro 	= 'PMI__Button__Event__CardiacFermiRoi'	$
	,	pro_set_value 	= 'PMI__Button__Control__CardiacFermiRoi' $
	, 	separator 	= separator						)
	return, id
end
