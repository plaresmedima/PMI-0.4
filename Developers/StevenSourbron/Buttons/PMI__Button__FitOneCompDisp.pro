pro PMI__Button__Event__FitOneCompDisp, ev

	if not TumorPixelAnalysis__input( $
		ev.top $
	,	Stdy 	= Stdy $
	,	status 	= status $
	,	time 	= time $
	,	pcurve 	= p $
	,	aif 	= aif $
	,	units 	= units $
	,	Roi 	= roi $
	,	Series 	= series $
	,	ev = ev $
	) 	then return

	PMI__Message, status, 'Preparing calculation..'

	d = Roi->d()
	n = mult(d[0:2])

	PT  = fltarr(n)
	PF 	= fltarr(n)
	PV 	= fltarr(n)
	TA  = fltarr(n)

   	i=0L
    ind = Roi -> Where(Stdy->DataPath(),t=Roi->t(i),n=n)
    while n eq 0 do begin
    	i=i+1
    	ind = Roi -> Where(Stdy->DataPath(),t=Roi->t(i),n=n)
    endwhile

	for i=0L,n-1 do begin

		PMI__Message, status, 'Fitting pixels to 1-compartment model with dispersion', i/(n-1.0)

		p[i,*] = FitOneCompDispDel(time,reform(p[i,*]),aif,Pars=Pars,Init=5,/nodelay,/mp,/noderivative,/quiet)

		PF[ind[i]] 	= 6000.0*Pars[0]
		PT[ind[i]] 	= Pars[1]/Pars[0]
		PV[ind[i]] 	= 100.0*Pars[1]
		TA[ind[i]] 	= Pars[2]

	endfor


	PMI__Message, status, 'Saving Results'

	Domain 	= {z:Series->z(), t:Series->t(0), m:Series->m()}

	S = Stdy->New('SERIES', Domain=Domain, Data=PF, Name='1 comp + disp - Plasma Flow (ml/100ml/min)')
	S = Stdy->New('SERIES', Domain=Domain, Data=PT, Name='1 comp + disp - Plasma MTT (sec)')
	S = Stdy->New('SERIES', Domain=Domain, Data=PV, Name='1 comp + disp - Plasma Volume (ml/100ml)')
	S = Stdy->New('SERIES', Domain=Domain, Data=TA, Name='1 comp + disp - Arterial MTT (sec)')

	return:PMI__Control, ev.top, /refresh
end


pro PMI__Button__Control__FitOneCompDisp, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 1)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__FitOneCompDisp, parent,value=value, separator=separator

	if n_elements(value) eq 0 then value = 'Pixel fit to single inlet 1-compartment model with dispersion'

	id = widget_button(parent $
	,	value 		= value $
	,	event_pro 	= 'PMI__Button__Event__FitOneCompDisp' $
	,	pro_set_value 	= 'PMI__Button__Control__FitOneCompDisp' $
	, 	separator 	= separator	)

	return, id

end
