pro PMI__Button__Event__TriggeredKidneyFiltrationRoi, ev

	PMI__Info, ev.top, Status=StatusId, Stdy=Stdy
    DynSeries = Stdy->Names(0,DefDim=3,ind=ind,sel=sel)

	in = PMI__Form(ev.top, Title='Filtration model with triggering', [$
	ptr_new({Type:'DROPLIST',Tag:'ser', Label:'Dynamic series', Value:DynSeries, Select:sel}), $
	ptr_new({Type:'DROPLIST',Tag:'aif', Label:'Arterial Region', Value:Stdy->names(1), Select:stdy->sel(1)}), $
	ptr_new({Type:'DROPLIST',Tag:'tri', Label:'Triggering Region', Value:Stdy->names(1), Select:stdy->sel(1)}), $
	ptr_new({Type:'DROPLIST',Tag:'roi', Label:'Tissue Region', Value:Stdy->names(1), Select:stdy->sel(1)}), $
	ptr_new({Type:'VALUE'	,Tag:'nb' , Label:'Length of baseline (# of dynamics)', Value:10L}),$
	ptr_new({Type:'VALUE'	,Tag:'hct', Label:'Patients hematocrit', Value:0.45})])
	IF in.cancel THEN return

	DceSeries = Stdy->Obj(0,ind[in.ser])

	Aif = PMI__RoiCurve(Stdy->DataPath(),DceSeries,Stdy->Obj(1,in.aif),StatusId)
	Roi = PMI__RoiCurve(Stdy->DataPath(),DceSeries,Stdy->Obj(1,in.roi),StatusId)
	Tri = PMI__RoiCurve(Stdy->DataPath(),DceSeries,Stdy->Obj(1,in.tri),StatusId,X=Time)

	Aif0 = total(Aif[0:in.nb-1])/in.nb
	Aif = (Aif-Aif0)/(1-in.hct)

	Ind = RetrospectiveTriggeringIndices(Tri, Time[1]-Time[0], 1.0/20.0, 100.0, /minima, cnt=cnt)

	nb = total(Time[ind] lt Time[in.nb-1])
	Roi0 = total(Roi[Ind[0:nb-1]])/nb
	Roi = Roi-Roi0

	P = [0.3, 120.0/6000, 2.0/3, 12/132.] ;[VP+VE, FP, VE/(VP+VE), FE/FP]
	Fit = FitSingleInlet('Filtration', Time-Time[0], Aif, Roi, P, INDICES=Ind, DELAY_PAR=Pd, DELAY_VALUES=[0,10,0.5], /POSITIVITY, LIMITED_ABOVE=[0,0,1,1])

	PMI__Control, ev.top, Viewer = 'PMI__Display__PerfusionRoiOutput', Display=Display

	Display -> Set, /Refresh $
	,	Model = 'Two-compartment filtration model' $
	,	Time = Time[Ind]-Time[0] $
	,	Curve = Roi[Ind] $
	,	Fit = Fit[Ind] $
	,	Units = Units $
	,	RoiName = (Stdy->Obj(1,in.roi))->Name() $
	,	Parameters = $
		[{Name:'Plasma Flow'		,Units:'ml/100ml/min'	,Value:6000D*P[1]			,Nr: 0,Rnd:1} $
		,{Name:'Plasma MTT'			,Units:'sec'			,Value:1D*P[0]*(1-P[2])/P[1],Nr: 1,Rnd:1} $
		,{Name:'Plasma Volume'		,Units:'ml/100ml'		,Value:100D*P[0]*(1-P[2])	,Nr: 2,Rnd:1} $
		,{Name:'Tubular Flow'		,Units:'ml/100ml/min'	,Value:6000D*P[1]*P[3]		,Nr: 4,Rnd:1} $
		,{Name:'Tubular MTT'		,Units:'sec'			,Value:1D*P[0]*P[2]/(P[1]*P[3])	,Nr: 5,Rnd:1} $
		,{Name:'Extraction Fraction',Units:'%' 				,Value:100D*P[3]			,Nr: 6,Rnd:1} $
		,{Name:'Arterial Delay'		,Units:'sec'			,Value:Double(Pd)			,Nr: 8,Rnd:1} ]

end

pro PMI__Button__Control__TriggeredKidneyFiltrationRoi, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 2)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__TriggeredKidneyFiltrationRoi, parent, value=value, separator=separator

	PMI__Display__PerfusionRoiOutput__Define

	if n_elements(value) eq 0 then value = 'Protocol with triggering (ROI)'

	return, widget_button(parent, $
		value 		= value, $
		event_pro 	= 'PMI__Button__Event__TriggeredKidneyFiltrationRoi',	$
		pro_set_value = 'PMI__Button__Control__TriggeredKidneyFiltrationRoi', $
	 	separator 	= separator	)

end
