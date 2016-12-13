pro PMI__Button__Event__FitFiltrationDimitraLLS, ev


	if not TumorPixelAnalysis__input( $
		ev.top $
	,	Stdy = Stdy $
	,	status = status $
	,	time = time $
	,	pcurve = p $
	,	aif = aif $
	,	units = units $
	,	Roi = roi $
	,	Series = series $
	,	ev =ev $
	) 	then return

	PMI__Message, status, 'Preparing calculation..'


	d = Roi->d()
	n = mult(d[0:2])
	PV 	= fltarr(n)
	PF 	= fltarr(n)
	PT 	= fltarr(n)
	TV 	= fltarr(n)
	TF 	= fltarr(n)
	TT 	= fltarr(n)


    i=0L
    ind = Roi -> Where(Stdy->DataPath(),t=Roi->t(i),n=n)
    while n eq 0 do begin
    	i=i+1
    	ind = Roi -> Where(Stdy->DataPath(),t=Roi->t(i),n=n)
    endwhile

	for i=0L,n-1 do begin

		PMI__Message, status, 'Fitting pixels to filtration model', i/(n-1.0)

		RENALMODEL_INVERSE_LLS_METHOD, reform(p[i,*]), aif, time, TPrec, TTrec, VPrec, VTrec

		j = ind[i]

		PV[j] 	= 100E*VPrec
		TV[j] 	= 100E*VTrec
		PF[j] 	= 6000E*VPrec/TPrec
		TF[j] 	= 6000E*VTrec/TTrec
		PT[j] 	= 1E*TPrec
		TT[j] 	= 1E*TTrec
	endfor

	Dom = {z:Series->z(), t:Series->t(0), m:Series->m()}

	PMI__Message, status, 'Saving Results'

	S = Stdy->New('SERIES',	Domain=Dom,	Data=PF, Name= 'FP (ml/100ml/min)')
	S = Stdy->New('SERIES',	Domain=Dom,	Data=PV, Name= 'VP (ml/100ml)')
	S = Stdy->New('SERIES',	Domain=Dom,	Data=PT, Name= 'TP (sec)')
	S = Stdy->New('SERIES',	Domain=Dom,	Data=TF, Name= 'FT (ml/100ml/min)' )
	S = Stdy->New('SERIES',	Domain=Dom,	Data=TV, Name= 'VT (ml/100ml)')
	S = Stdy->New('SERIES',	Domain=Dom,	Data=TT, Name= 'TT (sec)')

	PMI__Control, ev.top, /refresh
end


pro PMI__Button__Control__FitFiltrationDimitraLLS, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 1)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end


function PMI__Button__FitFiltrationDimitraLLS, parent,value=value, separator=separator


	if n_elements(value) eq 0 then value = 'Fit 2-compartment filtration model (Pixel)'

	id = widget_button(parent 						$
	,	value 		= value		$
	,	event_pro 	= 'PMI__Button__Event__FitFiltrationDimitraLLS'	$
	,	pro_set_value 	= 'PMI__Button__Control__FitFiltrationDimitraLLS' $
	, 	separator 	= separator						)

	return, id

end
