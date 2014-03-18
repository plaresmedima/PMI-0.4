pro PMI__Button__Event__KidneyFiltrationRoiStdAif, ev

	PMI__Info, ev.top, Status=StatusId, Stdy=Stdy
    DynSeries = Stdy->Names(0,DefDim=3,ind=ind,sel=sel)

	in = PMI__Form(ev.top, Title='Filtration model with standard AIF', [$
	ptr_new({Type:'DROPLIST',Tag:'ser', Label:'Dynamic series', Value:DynSeries, Select:sel}), $
	ptr_new({Type:'DROPLIST',Tag:'roi', Label:'Tissue Region', Value:Stdy->names(1), Select:stdy->sel(1)}), $
	ptr_new({Type:'VALUE'	,Tag:'nb' , Label:'Length of baseline (# of dynamics)', Value:10L}),$
	ptr_new({Type:'VALUE'	,Tag:'T1' , Label:'Tissue T1 (msec)', Value:700E})])
	IF in.cancel THEN return

	DceSeries = Stdy->Obj(0,ind[in.ser])

	Roi = PMI__RoiCurve(Stdy->DataPath(),DceSeries,Stdy->Obj(1,in.roi),StatusId,X=Time)
	Roi0 = total(Roi[0:in.nb-1])/in.nb
	Roi = (Roi-Roi0)/Roi0

	Time = Time-Time[0]

	Aif = ParkerAif(Time) ;Plasma concentration in mM
    Aif = 4.5 * (in.T1/1000E) * Aif ;Correction for kidney T1 (700ms) and relaxivity (4.5 Hz/mM)

	P = [0.3, 120.0/6000, 2.0/3, 12/132.] ;[VP+VE, FP, VE/(VP+VE), FE/FP]
	Fit = FitSingleInlet('Filtration', Time, Aif, Roi, P, DELAY_PAR=Pd, DELAY_VALUES=[0,30,1.0], /POSITIVITY, LIMITED_ABOVE=[0,0,1,1])

	PMI__Control, ev.top, Viewer = 'PMI__Display__PerfusionRoiOutput', Display=Display

	Display -> Set, /Refresh $
	,	Model = 'Two-compartment filtration model (Std AIF)' $
	,	Time = Time $
	,	Curve = Roi $
	,	Fit = Fit $
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

pro PMI__Button__Control__KidneyFiltrationRoiStdAif, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr ge 1)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__KidneyFiltrationRoiStdAif, parent, value=value, separator=separator

	PMI__Display__PerfusionRoiOutput__Define

	if n_elements(value) eq 0 then value = 'Kidney protocol with standardized AIF (ROI)'

	return, widget_button(parent, $
		value 		= value, $
		event_pro 	= 'PMI__Button__Event__KidneyFiltrationRoiStdAif',	$
		pro_set_value = 'PMI__Button__Control__KidneyFiltrationRoiStdAif', $
	 	separator 	= separator	)

end
