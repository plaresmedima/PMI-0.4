function PMI__Button__Input__TMUH_CT_FitExchangeRoi $
, 	top $
,	Status	 = id $
,	time	 = time $
,	curve 	 = roicurve $
,	aif 	 = aif $
,	ROI 	 = roi $
,	nb=nb $
,	ev = ev

	PMI__Info, top, Status=id, Stdy=Stdy
	PMI__Message, id, 'Getting input..'

   	Series = Stdy->Names(0,DefDim=3,ind=ind,sel=sel)

	in = PMI__Form(ev.top, Title='Perfusion analysis setup', [$
		ptr_new({Type:'DROPLIST',Tag:'ser', Label:'Dynamic series', Value:Series, Select:sel}), $
		ptr_new({Type:'DROPLIST',Tag:'aif', Label:'Arterial Region', Value:Stdy->names(1), Select:Stdy->sel(1)}), $
		ptr_new({Type:'DROPLIST',Tag:'roi', Label:'Tissue Region', Value:Stdy->names(1), Select:Stdy->sel(1)}), $
		ptr_new({Type:'VALUE'	,Tag:'nb' , Label:'Length of baseline (# of dynamics)', Value:5B}),$
		ptr_new({Type:'VALUE'	,Tag:'hct', Label:'Patients hematocrit', Value:0.45})])
		IF in.cancel THEN return, 0

    Series = Stdy->Obj(0,ind[in.ser])
    Art = Stdy->Obj(1,in.aif)
    Roi = Stdy->Obj(1,in.roi)
    nb = in.nb

    Time = Series->c(1)
    Time = Time-Time[0]

    Aif = PMI__RoiCurve(Stdy->DataPath(), Series, Art, id, cnt=cnt)
    if cnt eq 0 then begin
    	ok = dialog_message(/information,'Region <' + Art->Name() + '> is not defined on Series <' + Series->Name() + '>')
    	return, 0
    endif
	RoiCurve = PMI__RoiCurve(Stdy->DataPath(), Series, Roi, id, cnt=cnt)
    if cnt eq 0 then begin
    	ok = dialog_message(/information,'Region <' + Roi->Name() + '> is not defined on Series <' + Series->Name() + '>')
    	return, 0
    endif

	Aif = LMU__Enhancement(Aif,nb,relative=0)/(1-in.hct)
	RoiCurve = LMU__Enhancement(RoiCurve,nb,relative=0)

	return, 1

end


pro PMI__Button__Event__TMUH_CT_FitExchangeRoi, ev

  PMI__Info, ev.top, stdy=Stdy

  if not PMI__Button__Input__TMUH_CT_FitExchangeRoi($
     ev.top $
     ,	Status	= status $
     ,	time 	= time $
     ,	curve 	= curve $
     ,	aif		= aif $
     ,	Roi		= Roi $
     , 	ev=ev ) $
	then begin
		PMI__Message, Status
		return
     endif

	P = [0.3, 0.02, 2.0/3, 0.1] ;[VP+VE, FP, VE/(VP+VE), FE/(FP+FE)]
	Fit = FitSingleInlet('Exchange', time, aif, curve, P, AKAIKE_ERROR=aic, /POSITIVITY, LIMITED_ABOVE=[1,0,1,1])

	PMI__Control, ev.top, Viewer = 'PMI__Display__PerfusionRoiOutput', Display=Display

	Display -> Set, /Refresh $
	,	Model = 'Two-compartment exchange model' $
	,	Time = Time $
	,	Curve = Curve $
	,	Fit = Fit $
	,	Units = 'Signal Enhancement (HU)' $
	,	RoiName = Roi->Name() $
	,	Parameters = $
		[{Name:'Plasma Flow'					,Units:'ml/100ml/min'	,Value:6000D*P[1]			,Nr: 0,Rnd:1} $
		,{Name:'Plasma Mean Transit Time'		,Units:'sec'			,Value:1D*P[0]*(1-P[2])*(1-P[3])/P[1]	,Nr: 1,Rnd:1} $
		,{Name:'Plasma Volume'					,Units:'ml/100ml'		,Value:100.0*P[0]*(1-P[2])	,Nr: 2,Rnd:1} $
		,{Name:'EES Mean Transit Time'	,Units:'sec'			,Value:1D*(1-P[3])*P[0]*P[2]/(P[1]*P[3])	,Nr: 3,Rnd:1} $
		,{Name:'ESS Volume'				,Units:'ml/100ml'		,Value:100D*P[0]*P[2]		,Nr: 4,Rnd:1} $
		,{Name:'Extraction Fraction'				,Units:'%' 				,Value:100D*P[3]				,Nr: 5,Rnd:1} $
		,{Name:'Permeability-surface area product (PS)'				,Units:'ml/100ml/min'	,Value:6000D*P[1]*P[3]/(1-P[3])	,Nr: 6,Rnd:1} $
		,{Name:'Ktrans'				,Units:'ml/min/100ml'				,Value:6000D*P[3]*P[1]						,Nr:7,Rnd:1} ]

end

pro PMI__Button__Control__TMUH_CT_FitExchangeRoi, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 1)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__TMUH_CT_FitExchangeRoi, parent,value=value, separator=separator

	SingleInletExchange
	PMI__Display__PerfusionRoiOutput__Define

	if n_elements(value) eq 0 then value = 'Fit 2-compartment exchange model (ROI)'

	id = widget_button(parent 						$
	,	value 		= value		$
	,	event_pro 	= 'PMI__Button__Event__TMUH_CT_FitExchangeRoi'	$
	,	pro_set_value 	= 'PMI__Button__Control__TMUH_CT_FitExchangeRoi' $
	, 	separator 	= separator						)
	return, id
end
