function FitLiverRoiTmp__input $
, 	top $
,	Stdy	= Stdy $
,	Status	= id $
,	timeAIF	= timeAIF $
,	timeROI	= timeROI $
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
		[	'Dynamic series (ROI)' $
		,	'Dynamic series (AIF & VIF)' $
		,	'Arterial region (AIF)'$
		,	'Venous region (VIF)'$
		,	'Region of interest (ROI)'$
		,	'Approximate tracer concentrations by:'$
		,	'Length of baseline (# of dynamics)'$
		,	'Patients hematocrit'$
		]$
	,	ListNames=[Series,Series,Regions,Regions,Regions,Units] $
	, 	ListNumbers=[ns,ns,nr,nr,nr,2] $
	, 	ListDefaults=[sel,sel,Stdy->sel(1),Stdy->sel(1),Stdy->sel(1),0]$
	,	DataDefaults = {nb:10L, hct:0.45E} $
	) & if size(in,/type) eq 1 then return, 0

	SeriesROI = Stdy->Obj(0,ind[in.select[0]])
	SeriesAIF = Stdy->Obj(0,ind[in.select[1]])
    Art = Stdy->Obj(1,in.select[2])
    Ven = Stdy->Obj(1,in.select[3])
    Roi = Stdy->Obj(1,in.select[4])
    nB = in.data.nb

    TimeAIF = SeriesAIF->c(1)
    TimeAIF = TimeAIF-TimeAIF[0]

    TimeROI = SeriesROI->c(1)
    TimeROI = TimeROI-TimeROI[0]


	Units = Units[in.select[5]]

	RoiName = Roi->Name()

    Aif = PMI__RoiCurve(Stdy->DataPath(), SeriesAIF, Art, id, cnt=cnt)
    if cnt eq 0 then begin
    	ok = dialog_message(/information,'Arterial ROI is empty!')
    	return, 0
    endif
    Vif = PMI__RoiCurve(Stdy->DataPath(), SeriesAIF, Ven, id, cnt=cnt)
    if cnt eq 0 then begin
    	ok = dialog_message(/information,'Venous ROI is empty!')
    	return, 0
    endif
	Res = PMI__RoiCurve(Stdy->DataPath(), SeriesROI, Roi, id, cnt=cnt)
     if cnt eq 0 then begin
    	ok = dialog_message(/information,'Tissue ROI is empty!')
    	return, 0
    endif

    Aif = LMU__Enhancement(Aif,in.data.nb,relative=units eq 'Relative Signal Enhancement')/(1-in.data.hct)
    Vif = LMU__Enhancement(Vif,in.data.nb,relative=units eq 'Relative Signal Enhancement')/(1-in.data.hct)
    Res = LMU__Enhancement(Res,in.data.nb,relative=units eq 'Relative Signal Enhancement')

	return, 1

end
