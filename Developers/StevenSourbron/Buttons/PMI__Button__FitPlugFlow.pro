function PMI__Button__Input__FitPlugFlow $
,	ev $
,	Stdy = Stdy $
,	status = status $
,	time = time $
,	pcurve = pcurve $
,	aif = aif $
,	Roi = roi $
,	Series = series $
,	Delay	= Delay

	PMI__Info, ev.top, Status=Status, Stdy=Stdy

	Series = Stdy->Names(0,ns,DefDim=3,ind=ind,sel=sel)
	Regions = Stdy->names(1,nr)

	Units = ['Signal Enhancement','Relative Signal Enhancement']

	in = cw_InputForm(/pos, ev=ev $
	,	Title 	= 'Fit plug flow model' $
	,	Labels 	= $
		[	'Dynamic series' $
		,	'Arterial region'$
		,	'Region of interest'$
		,	'Approximate tracer concentrations by:'	$
		,	'Length of baseline (# of dynamics)'$
		,	'Patients hematocrit'$
		,	'Maximal Transit Time (sec)' $
		,	'Transit Time Precision (sec)' $
		]$
	,	ListNames = [ Series,Regions,Regions,Units ]$
	,	ListNumbers 	= [	ns,nr,nr,2]$
	,	ListDefaults 	= [	sel,Stdy->sel(1),Stdy->sel(1),0]$
	,	DataDefaults 	= {	nb:10L, hct:0.45, Tmax:10E, Tres:2.0}$
	) & if size(in,/type) eq 1 then return, 0

	Series = Stdy->Obj(0,ind[in.select[0]])
	Art 	= Stdy->Obj(1,in.select[1])
	Roi 	= Stdy->Obj(1,in.select[2])
	Units 	= Units[in.select[3]]

	Delay = -in.data.Tmax + in.data.Tres*findgen(1+floor(2*in.data.Tmax/in.data.Tres))

	Time = Series->c(1)
	Time = Time-Time[0]

	Aif = PMI__RoiCurve(Stdy->DataPath(), Series, Art, Status, cnt=cnt) & if cnt eq 0 then return, 0
	Aif = LMU__Enhancement(Aif,in.data.nb,relative=in.select[3] eq 1)/(1-in.data.hct)

	pcurve = PMI__PixelCurve(Stdy->DataPath(), Series, Roi, status, cnt=cnt) & if cnt eq 0 then return, 0
	for i=0L,cnt-1 do begin
		PMI__Message, status, 'Calculating '+ Units, i/(cnt-1E)
		pcurve[i,*] = LMU__Enhancement(pcurve[i,*], in.data.nb, relative=in.select[3] eq 1)
	endfor

	return, 1
end

pro PMI__Button__Event__FitPlugFlow, ev

	if not PMI__Button__Input__FitPlugFlow( $
		ev $
	,	Stdy 	= Stdy $
	,	status 	= status $
	,	time 	= time $
	,	pcurve 	= p $
	,	aif 	= aif $
	,	Roi 	= roi $
	,	Series 	= series $
	,	Delay	= DelayInit $
	) 	then return

	PMI__Message, status, 'Preparing calculation..'

	d = Roi->d()
	n = mult(d[0:2])

	PV 	= fltarr(n)
	TA  = fltarr(n)

    i=0L
    ind = Roi -> Where(Stdy->DataPath(),t=Roi->t(i),n=n)
    while n eq 0 do begin
    	i=i+1
    	ind = Roi -> Where(Stdy->DataPath(),t=Roi->t(i),n=n)
    endwhile

	for i=0L,n-1 do begin

		PMI__Message, status, 'Fitting pixels to plug flow outlet model', i/(n-1.0)

		Delay = DelayInit
		Fit = FitPureDelay(time,reform(p[i,*]),aif,Delay=Delay,Pars=Pars)

		TA[ind[i]] 	= 1000.0*Delay
		PV[ind[i]] 	= 100.0*Pars[0]
	endfor

	PMI__Message, status, 'Saving Results'

	Domain 	= {z:Series->z(), t:Series->t(0), m:Series->m()}

	S = Stdy->New('SERIES',	Domain=Domain,Data=PV, ClrWin=[0E,100E] , Name='Plug flow outlet - Plasma Volume (ml/100ml)')
	S = Stdy->New('SERIES',	Domain=Domain,Data=TA, ClrWin=[0E,1000E], Name='Plug flow outlet - Arterial Transit Time (msec)')

	return:PMI__Control, ev.top, /refresh
end

pro PMI__Button__Control__FitPlugFlow, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 1)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__FitPlugFlow, parent,value=value, separator=separator

	if n_elements(value) eq 0 then value = 'Pixel fit to plug flow outlet model'

	id = widget_button(parent $
	,	value 		= value $
	,	event_pro 	= 'PMI__Button__Event__FitPlugFlow' $
	,	pro_set_value 	= 'PMI__Button__Control__FitPlugFlow' $
	, 	separator 	= separator	)

	return, id

end
