pro PMI__Button__Event__FitFiltration, ev


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

	    Tpinit = 3.0D
	    TTinit = 60.0D
	    VPinit = 0.1D
	    VTinit = 0.3D

		Pars = [VPinit+VTinit, VPinit/Tpinit, VTinit/(VPinit+VTinit), (VTinit/TTinit)/(VPinit/Tpinit)] ;[VP+VE, FP, VE/(VP+VE), FE/FP]
		Fit = FitSingleInlet('Filtration', time, aif, reform(p[i,*]), Pars)

		j = ind[i]

		PV[j] 	= 100E*Pars[0]*(1-Pars[2])
		TV[j] 	= 100E*Pars[0]*Pars[2]
		PF[j] 	= 6000.0*Pars[1]
		TF[j] 	= 6000E*Pars[1]*Pars[3]
		PT[j] 	= 1E*Pars[0]*(1-Pars[2])/Pars[1]
		TT[j] 	= 1E*Pars[0]*Pars[2]/(Pars[1]*Pars[3])
	endfor

	Dom = {z:Series->z(), t:Series->t(0), m:Series->m()}

	PMI__Message, status, 'Saving Results'

	S = Stdy->New('SERIES',	Domain=Dom,	Data=PF, Name= 'Plasma Flow (ml/100ml/min)')
	S = Stdy->New('SERIES',	Domain=Dom,	Data=PV, Name= 'Plasma Volume (ml/100ml)')
	S = Stdy->New('SERIES',	Domain=Dom,	Data=PT, Name= 'Plasma MTT (sec)')
	S = Stdy->New('SERIES',	Domain=Dom,	Data=TF, Name= 'Permeability-surface area product (PS)' )
	S = Stdy->New('SERIES',	Domain=Dom,	Data=TV, Name= 'ESS Volume (ml/100ml)')
	S = Stdy->New('SERIES',	Domain=Dom,	Data=TT, Name= 'EES MTT (sec)')

	PMI__Control, ev.top, /refresh
end

pro PMI__Button__Control__FitFiltration, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 1)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__FitFiltration, parent,value=value, separator=separator

	if n_elements(value) eq 0 then value = 'Fit 2-compartment filtration model (Pixel)'

	id = widget_button(parent 						$
	,	value 		= value		$
	,	event_pro 	= 'PMI__Button__Event__FitFiltration'	$
	,	pro_set_value 	= 'PMI__Button__Control__FitFiltration' $
	, 	separator 	= separator						)

	return, id

end
