function PMI__Button__Input__ArshadFermiRoiStressRest, top, status=Status, $
  tStress = tStress,  tRest=tRest, $
  AifStress = AifStress, AifRest = AifRest, $
  RoiStress = RoiStress, RoiRest = RoiRest, $
  Units = Units

	;GET USER INPUT

    PMI__Info, top, Status=Status, Stdy=Stdy
    Series = Stdy->Names(0,DefDim=3,ind=ind,sel=sel)

	in = PMI__Form(top, Title='Cardiac Perfusion Analysis', [$
	ptr_new({Type:'DROPLIST',Tag:'dyns', Label:'Stress data'	, Value:Series, Select:sel}), $
	ptr_new({Type:'DROPLIST',Tag:'dynr', Label:'Rest data'		, Value:Series, Select:sel}), $
	ptr_new({Type:'DROPLIST',Tag:'aifs', Label:'Stress AIF'	, Value:Stdy->names(1), Select:stdy->sel(1)}), $
	ptr_new({Type:'DROPLIST',Tag:'aifr', Label:'Rest AIF'  	, Value:Stdy->names(1), Select:stdy->sel(1)}), $
	ptr_new({Type:'DROPLIST',Tag:'rois', Label:'Stress ROI'		, Value:Stdy->names(1), Select:stdy->sel(1)}), $
	ptr_new({Type:'DROPLIST',Tag:'roir', Label:'Rest ROI'		, Value:Stdy->names(1), Select:stdy->sel(1)}), $
	ptr_new({Type:'VALUE'	,Tag:'nbs' , Label:'Stress baseline'	, Value:2}), $
	ptr_new({Type:'VALUE'	,Tag:'nbr' , Label:'Rest baseline'		, Value:2}), $
	ptr_new({Type:'DROPLIST',Tag:'seq' , Label:'Signal model:', Value:['Linear','Dual-phase (T1b = 1.4s)','CE-MARC(T1b = 1.4s)'], Select:0})])
	IF in.cancel THEN return, 0

    DynS = Stdy->Obj(0,ind[in.dyns])
    DynR = Stdy->Obj(0,ind[in.dynr])
    ArtS = Stdy->Obj(1,in.aifs)
    ArtR = Stdy->Obj(1,in.aifr)
    RoiS = Stdy->Obj(1,in.rois)
    RoiR = Stdy->Obj(1,in.roir)

    tStress = DynS->c(1)
    tRest = DynR->c(1)
    tStress = tStress-tStress[0]
    tRest = tRest-tRest[0]


	;GET ROI CURVES

    AifStress = PMI__RoiCurve(Stdy->DataPath(), DynS, ArtS, Status, cnt=cnt, slice=AIFsliceS)
    if cnt eq 0 then begin
    	ok = dialog_message(/information,'Arterial region (Stress) is empty')
    	return, 0
    end
    AifRest = PMI__RoiCurve(Stdy->DataPath(), DynR, ArtR, Status, cnt=cnt, slice=AIFsliceR)
    if cnt eq 0 then begin
    	ok = dialog_message(/information,'Arterial region (Rest) is empty')
    	return, 0
    end
	RoiStress = PMI__RoiCurve(Stdy->DataPath(), DynS, RoiS, Status, cnt=cnt, slice=ROIsliceS)
    if cnt eq 0 then begin
    	ok = dialog_message(/information,'Region <' + RoiStress->Name() + '> is not defined on Series <' + DynS->Name() + '>')
    	return, 0
    endif
	RoiRest = PMI__RoiCurve(Stdy->DataPath(), DynR, RoiR, Status, cnt=cnt, slice=ROIsliceR)
    if cnt eq 0 then begin
    	ok = dialog_message(/information,'Region <' + RoiRest->Name() + '> is not defined on Series <' + DynR->Name() + '>')
    	return, 0
    endif

	;No longer necessary since PMI update from 28/07/2011: Philips data are scaled correctly on import

;	;CORRECT SCALING FACTORS
;
;	b = float(DynS -> get('0028'x,'1052'x))
;	a = float(DynS -> get('0028'x,'1053'x))
;	if a gt 0 then begin
;		AifStress = (AifStress - b)/a
;		RoiStress = (RoiStress - b)/a
;	endif
;	b = float(DynR -> get('0028'x,'1052'x))
;	a = float(DynR -> get('0028'x,'1053'x))
;	if a gt 0 then begin
;		AifRest = (AifRest - b)/a
;		RoiRest = (RoiRest - b)/a
;	endif

;	;CORRECT COIL LOADING
;
;	b = float(DynS -> get('2005'x,'100D'x))
;	a = float(DynS -> get('2005'x,'100E'x))
;	if a gt 0 then begin
;		AifStress = (AifStress - b)/a/1000.
;		RoiStress = (RoiStress - b)/a/1000.
;	endif
;	b = float(DynR -> get('2005'x,'100D'x))
;	a = float(DynR -> get('2005'x,'100E'x))
;	if a gt 0 then begin
;		AifRest = (AifRest - b)/a/1000.
;		RoiRest = (RoiRest - b)/a/1000.
;	endif


    ;CALCULATE CONCENTRATIONS

    RefSig = total(AifStress[0:in.nbs-1])/in.nbs
    RefT1 = 1400.0
    relaxivity = 4.5
	case in.seq of
		0:units = 'Signal Enhancement (a.u.)'
		1:begin
			units = 'Concentration (mM)'
			AifStress 	= 1000/T1_CardiacDualPhaseLeeds(AifStress	,RefSig, RefT1)/relaxivity
			RoiStress 	= 1000/T1_CardiacDualPhaseLeeds(RoiStress	,RefSig, RefT1)/relaxivity
			AifRest 	= 1000/T1_CardiacDualPhaseLeeds(AifRest		,RefSig, RefT1)/relaxivity
			RoiRest 	= 1000/T1_CardiacDualPhaseLeeds(RoiRest		,RefSig, RefT1)/relaxivity
		end
		2:begin
			units = 'Concentration (mM)'
			AifStress 	= 1000/T1_CEMARC(AifStress	,RefSig, RefT1, DynS->d(2)-1 - AIFsliceS)/relaxivity
			RoiStress 	= 1000/T1_CEMARC(RoiStress	,RefSig, RefT1, DynS->d(2)-1 - ROIsliceS)/relaxivity
			AifRest 	= 1000/T1_CEMARC(AifRest	,RefSig, RefT1, DynR->d(2)-1 - AIFsliceR)/relaxivity
			RoiRest 	= 1000/T1_CEMARC(RoiRest	,RefSig, RefT1, DynR->d(2)-1 - ROIsliceR)/relaxivity
		end
	endcase
	AifStress 	= LMU__Enhancement(AifStress	,in.nbs ,relative=0)/0.55
	AifRest 	= LMU__Enhancement(AifRest		,in.nbr ,relative=0)/0.55
	RoiStress 	= LMU__Enhancement(RoiStress	,in.nbs ,relative=0)
    RoiRest 	= LMU__Enhancement(RoiRest		,in.nbr ,relative=0)

  	return, 1
end

pro PMI__Button__Event__ArshadFermiRoiStressRest, ev

    if not PMI__Button__Input__ArshadFermiRoiStressRest(ev.top, Status=Status, $
      	tStress = tStress,  tRest = tRest, $
      	AifStress = AifStress, AifRest = AifRest, $
      	RoiStress = RoiStress, RoiRest = RoiRest, $
      	units = units )  $
    then begin
     	PMI__Message, Status
     	return
    endif

	plot, tStress, AIFstress
	oplot, tRest, AIFrest

	PMI__Message, status, 'Fitting Fermi model...'

	Ps = [0.015, 1.0, 0.5] ;[FP, a, b]
	FitStress = FitSingleInlet('Fermi', tStress, AifStress, RoiStress, Ps, DELAY_PAR=Pds, DELAY_VALUES=[0,5,0.25], /noderivative)
	Pr = [0.015, 1.0, 0.5] ;[FP, a, b]
	FitRest = FitSingleInlet('Fermi', tRest, AifRest, RoiRest, Pr, DELAY_PAR=Pdr, DELAY_VALUES=[0,5,0.25], /noderivative)

	ParsStress 	= [	{Name:'Blood Flow (Stress)'	,Units:'ml/g/min'	,Value:60D*Ps[0]/0.55	,Nr:0,Rnd:1}]
	ParsRest 	= [	{Name:'Blood Flow (Rest)'	,Units:'ml/g/min'	,Value:60D*Pr[0]/0.55	,Nr:1,Rnd:1},$
		 			{Name:'Perfusion Reserve'	,Units:''			,Value:1D*Ps[0]/Pr[0]	,Nr:2,Rnd:1}]

	PMI__Control, ev.top, Viewer = 'PMI__Display__RoiFit', Display=Display
	Display -> Plot, tStress, RoiStress, FitStress, ParsStress, $
		title='Fermi model with T1-correction', xtitle='Time (sec)', ytitle=units, $
		Xrange = [0,max([max(tStress),max(tRest)])], $
		Yrange = [min([min(RoiStress),min(RoiRest)]), max([max(RoiStress),max(RoiRest)])]
	Display -> oPlot, tRest, RoiRest, FitRest, ParsRest

end

pro PMI__Button__Control__ArshadFermiRoiStressRest, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns ge 2) and (nr ge 4)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__ArshadFermiRoiStressRest, parent,value=value, separator=separator

	SingleInletFermi
	PMI__Display__RoiFit__Define

	if n_elements(value) eq 0 then value = 'Fit Fermi model with T1-correction (ROI)'

	return,widget_button(parent, value=value, separator=separator,	$
		event_pro = 'PMI__Button__Event__ArshadFermiRoiStressRest',	$
		pro_set_value = 'PMI__Button__Control__ArshadFermiRoiStressRest')
end
