function PMI__Button__Input__ArshadDeconvolutionRoi $
   ,    ev $
   ,    Stdy = Stdy $
   ,    status = status $
   ,    time = time $
   ,    aif = aif $
   ,	curve = curve $
   ,	Roi =Roi

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

    Aif = PMI__RoiCurve(Stdy->DataPath(), Series, Art, status, cnt=cnt)
    if cnt eq 0 then begin
    	ok = dialog_message(/information,'Arterial region is empty')
    	return, 0
    end
    Aif = LMU__Enhancement(Aif,in.nb,relative=in.rel)/0.55

	Curve = PMI__RoiCurve(Stdy->DataPath(), Series, Roi, status, cnt=cnt)
    if cnt eq 0 then begin
    	ok = dialog_message(/information,'Region <' + Roi->Name() + '> is not defined on Series <' + Series->Name() + '>')
    	return, 0
    endif
    Curve = LMU__Enhancement(Curve,in.nb,relative=in.rel)

  	return, 1
end

pro PMI__Button__Event__ArshadDeconvolutionRoi, ev

    if not PMI__Button__Input__ArshadDeconvolutionRoi(ev $
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

	PMI__Message, status, 'Deconvolving..'

	IRF = DeconvolveCurve(time,	curve, aif $
	,	pc='LCC', wm=1L, m0=0.01, m1=1.0, nm=100L, Quad='O2' $
	,	dt=dt, CurveRegr=c, Fit=Fit)

	PMI__Control, ev.top, Viewer = 'PMI__Display__PerfusionRoiOutput', Display=Display

	Display -> Set, /Refresh $
	,	Model = 'Deconvolution Analysis' $
	,	Time = dt*findgen(n_elements(c)) $
	,	Curve = c $
	,	Fit = Fit $
;	,	Curve = IRF $
;	,	Fit = IRF $
	,	Units = 'Signal Enhancement' $
	,	RoiName = Roi->Name() $
	,	Parameters = $
		[ {Name:'Blood Flow'			,Units:'ml/100ml/min'	,Value:6000.0*max(IRF)/0.55		,Nr:0,Rnd:1} $
		, {Name:'Mean Transit Time'		,Units:'sec'			,Value:dt*total(IRF)/max(IRF)	,Nr:1,Rnd:1} $
		, {Name:'Extracellular Volume'	,Units:'ml/100ml'		,Value:100.0*dt*total(IRF) 		,Nr:2,Rnd:1} ]

end



pro PMI__Button__Control__ArshadDeconvolutionRoi, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->names(1,nr)
		sensitive = (ns gt 0) and (nr gt 1)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__ArshadDeconvolutionRoi, parent,value=value,separator=separator

	PMI__Display__PerfusionRoiOutput__Define

	if n_elements(value) eq 0 then value = 'Deconvolution analysis (ROI)'

    id = widget_button(parent $
    , 	value 		= value $
	, 	event_pro	= 'PMI__Button__Event__ArshadDeconvolutionRoi'	$
	,	pro_set_value 	= 'PMI__Button__Control__ArshadDeconvolutionRoi' $
	, 	separator 	= separator	)

	return, id
end

