function FitLiverRoi__input $
, 	top $
,	Stdy	= Stdy $
,	Status	= id $
,	time	= time $
,	curve 	= res $
,	aif 	= aif $
,	vif		= vif $
,	units 	= units $
,	RoiName = RoiName $
,	Series	= Series $
,	Region	= Roi $
,	nB		= nB $
,	ev = ev

	PMI__Info, top, Status=id, Stdy=Stdy

    Series = Stdy->Names(0,ns,DefDim=3,ind=ind,sel=sel)
    Regions = Stdy->names(1,nr)

	Units = ['Signal Enhancement','Relative Signal Enhancement']

	in = cw_InputForm(/pos,ev=ev	$
	,	Title 	= 'Perfusion analysis setup' $
	,	Labels 	= $
		[	'Dynamic series' $
		,	'Arterial region'$
		,	'Venous region'$
		,	'Region of interest'$
		,	'Approximate tracer concentrations by:'$
		,	'Length of baseline (# of dynamics)'$
		,	'Patients hematocrit'$
		]$
	,	ListNames=[Series,Regions,Regions,Regions,Units] $
	, 	ListNumbers=[ns,nr,nr,nr,2] $
	, 	ListDefaults=[sel,Stdy->sel(1),Stdy->sel(1),Stdy->sel(1),0]$
	,	DataDefaults = {nb:10L, hct:0.45E} $
	) & if size(in,/type) eq 1 then return, 0

	Series = Stdy->Obj(0,ind[in.select[0]])
    Art = Stdy->Obj(1,in.select[1])
    Ven = Stdy->Obj(1,in.select[2])
    Roi = Stdy->Obj(1,in.select[3])
    nB = in.data.nb

    Time = Series->c(1)
    Time = Time-Time[0]

	Units = Units[in.select[4]]

	RoiName = Roi->Name()

    Aif = PMI__RoiCurve(Stdy->DataPath(), Series, Art, id, cnt=cnt)
    if cnt eq 0 then begin
    	ok = dialog_message(/information,'Arterial ROI is empty!')
    	return, 0
    endif
    Vif = PMI__RoiCurve(Stdy->DataPath(), Series, Ven, id, cnt=cnt)
    if cnt eq 0 then begin
    	ok = dialog_message(/information,'Venous ROI is empty!')
    	return, 0
    endif
	Res = PMI__RoiCurve(Stdy->DataPath(), Series, Roi, id, cnt=cnt)
     if cnt eq 0 then begin
    	ok = dialog_message(/information,'Tissue ROI is empty!')
    	return, 0
    endif

    Aif = LMU__Enhancement(Aif,in.data.nb,relative=units eq 'Relative Signal Enhancement')/(1-in.data.hct)
    Vif = LMU__Enhancement(Vif,in.data.nb,relative=units eq 'Relative Signal Enhancement')/(1-in.data.hct)
    Res = LMU__Enhancement(Res,in.data.nb,relative=units eq 'Relative Signal Enhancement')

	return, 1

end
