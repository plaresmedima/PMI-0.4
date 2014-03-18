function DualInletPerfusionAnalysis__input $
, 	top $
,	Stdy	 = Stdy $
,	Status	 = status $
,	time	 = time $
,	aif 	 = aif $
,	vif		 = vif $
,	units 	 = units $
,	Roi 	 = roi $
,	pixelcurve = pcurve $
,	ev = ev


	if not FitLiverRoi__input($
		ev.top $
	, 	Stdy	= Stdy $
	,	status 	= status $
	,	time 	= time $
	,	curve 	= curve $
	,	aif		= aif $
	,	vif 	= vif $
	,	Units	= units $
	,	ev		= ev $
	,	Series	= Series $
	,	Region	= Roi $
	,	nB		= nB $
	) then return, 0


	pcurve = PMI__PixelCurve(Stdy->DataPath(), Series, Roi, id, cnt=cnt)
	if cnt eq 0 then return, 0
	np = n_elements(pcurve[*,0])
	for i=0L,np-1 do begin
		PMI__Message, id, 'Calculating '+ Units + ' ', i/(np-1E)
		pcurve[i,*] = LMU__Enhancement(pcurve[i,*],nb,relative=Units eq 'Relative Signal Enhancement')
	endfor

	return, 1

end

