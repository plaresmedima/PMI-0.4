pro PMI__Button__Event__FitModToftsDimitraLLSmodelfit, ev


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


	d = Series->d()
	n = mult(d[0:2])
	fit = fltarr(n,d[3])

    i=0L
    ind = Roi -> Where(Stdy->DataPath(),t=Roi->t(i),n=n)
    while n eq 0 do begin
    	i=i+1
    	ind = Roi -> Where(Stdy->DataPath(),t=Roi->t(i),n=n)
    endwhile

	for i=0L,n-1 do begin

		PMI__Message, status, 'Fitting pixels to Modified Tofts model', i/(n-1.0)

		FitModifiedToftsLinear, time, aif, reform(p[i,*]), vp=vp, ve=ve, Ktrans=Ktrans
		SingleInletModifiedTofts, [d[3], lindgen(d[3]), time, aif], [vp+ve, ve/(vp+ve), Ktrans], Ctt

		fit[ind[i],*] = Ctt

	endfor

	PMI__Message, status, 'Saving Results'

	S = Stdy->New('SERIES',	Default=Series,	Data=fit, Name= 'ModTofts Fit')

	PMI__Control, ev.top, /refresh
end


pro PMI__Button__Control__FitModToftsDimitraLLSmodelfit, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 1)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end


function PMI__Button__FitModToftsDimitraLLSmodelfit, parent,value=value, separator=separator


	if n_elements(value) eq 0 then value = 'Fit Modified Tofts model (Pixel)'

	id = widget_button(parent 						$
	,	value 		= value		$
	,	event_pro 	= 'PMI__Button__Event__FitModToftsDimitraLLSmodelfit'	$
	,	pro_set_value 	= 'PMI__Button__Control__FitModToftsDimitraLLSmodelfit' $
	, 	separator 	= separator						)

	return, id

end
