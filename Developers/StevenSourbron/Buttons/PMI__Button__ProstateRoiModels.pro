function PMI__Button__ProstateRoiModels__input $
, 	top $
,	Status	= id $
, 	Stdy	= Stdy $
,	time 	= time $
,	curve 	= roicurve $
,	aif		= aif $
,	RoiName	= RoiName

	PMI__Info, top, Status=id, Stdy=Stdy

    Series = Stdy->names(0,ns,DefDim=3,ind=ind,sel=sel)
    if ns eq 0 then begin
       ok = dialog_message(/information,'Please load a dynamic series')
       return, 0
    endif
    Regions = Stdy->names(1,nr)
    if nr eq 0 then begin
       ok = dialog_message(/information,'Please define arterial- and tissue regions')
       return, 0
    endif

	in = cw_InputForm(										$
		Title 	= 'Perfusion analysis setup' $
	,	Labels 	= 											$
		[	'Dynamic series' 								$
		,	'Arterial region'								$
		,	'Region of interest'							$
		,	'Length of baseline (# of dynamics)'			$
		,	'Patients hematocrit'							$
		]													$
	,	ListNames 		= [ Series,Regions,Regions]$
	,	ListNumbers 	= [	ns,nr,nr]$
	,	ListDefaults 	= [	sel,Stdy->sel(1),Stdy->sel(1)]$
	,	DataDefaults 	= {	nb:8L,	hct:0.45E}$
	) & if size(in,/type) eq 1 then return, 0

	Series = Stdy->Obj(0,ind[in.select[0]])
	Art = Stdy->Obj(1,in.select[1])
	Roi = Stdy->Obj(1,in.select[2])

	Time = Series->c(1)
	Time = Time-Time[0]

	Aif = PMI__RoiCurve(Stdy->DataPath(), Series, Art, id, cnt=cnt)
    if cnt eq 0 then begin
    	ok = dialog_message(/information,'Arterial region is empty')
    	return, 0
    end
	Aif = LMU__Enhancement(Aif,in.data.nb,/relative)
	Aif = Aif/(1-in.data.hct)

  	cw_create_plot, Aif $
  	,	xaxis 	= Time $
	,	xtitle 	= 'Time (/sec)' $
	,	ytitle 	= 'Relative Signal Enhancement' $
	,	title 	= 'Arterial Input Function'

	RoiName = Roi->Name()
	RoiCurve = PMI__RoiCurve(Stdy->DataPath(), Series, Roi, id, cnt=cnt)
    if cnt eq 0 then begin
    	ok = dialog_message(/information,'Tissue region is empty')
    	return, 0
    end
	RoiCurve = LMU__Enhancement(RoiCurve,in.data.nb,/relative)

	return, 1
end



pro PMI__Button__ProstateRoiModels__event, ev

	PMI__Info, ev.top, Stdy=Stdy

	if not PMI__Button__ProstateRoiModels__input($
		ev.top $
	,	status	= status $
	,	time 	= time $
	,	curve 	= roicurve $
	,	aif		= aif $
	,	RoiName	= RoiName $
	) then goto, return


	PMI__Message, Status, 'Deconvolving'
	IRF = DeconvolveCurve(time,roicurve,aif,dt=dt,Fit=FitDec)
	PFdec = 6000.0*max(IRF)
	VDdec = 100.0*dt*total(IRF)
	TTdec = dt*total(IRF)/max(IRF)
	CSdec = 100.0*total((roicurve-FitDec)^2)/total(roicurve^2)


	PMI__Message, Status, 'Fitting 1-compartment model'
	FitOne = FitKetyDel(time,roicurve,aif,init=5,Delay=TAone,Pars=P,/mp,/noderivative,/quiet)
	PFone = 6000.0*P[0]
	VDone = 100.0*P[1]
	TTone = P[1]/P[0]
	CSone = 100.0* total((roicurve-FitOne)^2)/total(roicurve^2)


	PMI__Message, Status, 'Fitting 2-compartment model (Patlak)'
	FitPat = FitPatlakNonlinearDelay(time,roicurve,aif,init=5,Delay=TApat,Pars=P,/mp,/noderivative,/quiet)
 	PVpat = 100.0*P[0]
	EFpat = 6000.0*P[1]
	CSpat = 100.0*total((roicurve-FitPat)^2)/total(roicurve^2)


	PMI__Message, Status, 'Fitting 2-compartment model (Uptake)'
	FitUpt = FitSingleInletUptakeDelay(Time,RoiCurve,Aif,Pars=Pk,Init=5,Delay=TAupt,Convective=Pc,/mp,/noderivative,/quiet)
	PFupt = 6000.0*Pc[2]
	PTupt = Pk[1]
	PVupt = 100.0*Pc[1]
	EFupt = 6000.0*Pc[0]
	EXupt = 100.0*Pc[0]/Pc[2]
	CSupt = 100.0*total((roicurve-FitUpt)^2)/total(roicurve^2)


	PMI__Message, Status, 'Fitting 2-compartment model (Tofts)'
	FitTof = FitToftsDelay(time,roicurve,aif,Init=5,Delay=TAtof,Pars=P,Convective=Pc,/mp,/noderivative,/quiet)
 	PVtof = 100.0*Pc[1]
	EFtof = 6000.0*Pc[0]
	ITtof = 1/P[2]
	IVtof = 100.0*Pc[2]
	CStof = 100.0*total((roicurve-FitTof)^2)/total(roicurve^2)


	PMI__Message, Status, 'Fitting 2-compartment model (Mammillary)'
	FitMam = TwoCompartmentDelayFit_Indirect(Time,RoiCurve,Aif,1,Init=5,Delay=TAmam,Kinetic=Pk,Convective=Pc,/mp,/noderivative,/quiet)
    PVmam = 100.0*Pc[2]
    IVmam = 100.0*Pc[3]
    PFmam = 6000.0*Pc[4]
    EFmam = 6000.0*Pc[0]
    PTmam = Pk[2]
    ITmam = Pk[3]
    EXmam = 100.0*Pc[0]/Pc[4]
    CSmam = 100.0*total((roicurve-FitMam)^2)/total(roicurve^2)



	;Plot graphical output



	window, xsize=1200, ysize=800

	MinC = min([min(roicurve),min(FitDec),min(FitOne),min(FitUpt),min(FitTof),min(FitMam)])
	MaxC = max([max(roicurve),max(FitDec),max(FitOne),max(FitUpt),max(FitTof),max(FitMam)])

 	plot, /nodata, position=[0.1,0.1,0.6,0.90]  $
 	,	Title = RoiName $
	, 	[0,max(Time)], [MinC,MaxC] $
	, 	/xstyle, /ystyle $
	, 	background=255, color=0 $
	, 	xtitle = 'Time (sec)', ytitle='Relative Signal Enhancement' $
	, 	charsize=2.0, charthick=2.0, xthick=2.0, ythick=2.0

	loadct, 12

	tDec = dt*findgen(n_elements(FitDec))

	oplot, Time, roicurve	, color=14*16	, linestyle = 0, thick=2
	oplot, tDec, FitDec		, color=0*16	, linestyle = 0, thick=2
	oplot, Time, FitOne		, color=6*16	, linestyle = 0, thick=2
	oplot, Time, FitPat		, color=7*16	, linestyle = 0, thick=2
	oplot, Time, FitUpt		, color=9*16	, linestyle = 0, thick=2
	oplot, Time, FitTof		, color=2*16	, linestyle = 0, thick=2
	oplot, Time, FitMam		, color=12*16	, linestyle = 0, thick=2


	py=0.975 & dy=0.0220 & x0=0.625 & charsize=1.5 & charthick=1.5

	           xyouts,x0,py,'MODEL-FREE DECONVOLUTION'	,color=0*16,/normal,charsize=charsize,charthick=charthick
	py=py-dy & xyouts,x0,py,'Plasma Flow = '			+PMI__Round(PFdec,2)+' ml/100ml/min',color=0,/normal,charsize=charsize,charthick=charthick
	py=py-dy & xyouts,x0,py,'Volume of Distribution = '	+PMI__Round(VDdec,2)+' ml/100ml'	,color=0,/normal,charsize=charsize,charthick=charthick
	py=py-dy & xyouts,x0,py,'Mean Transit Time = '		+PMI__Round(TTdec,2)+' sec'			,color=0,/normal,charsize=charsize,charthick=charthick
	py=py-dy & xyouts,x0,py,'Chi-Square fit error = '	+PMI__Round(CSdec,3)+' %'			,color=0,/normal,charsize=charsize,charthick=charthick
	py=py-dy
	py=py-dy & xyouts,x0,py,'ONE COMPARTMENT'			,color=6*16,/normal,charsize=charsize,charthick=charthick
	py=py-dy & xyouts,x0,py,'Plasma Flow = '			+PMI__Round(PFone,2)+' ml/100ml/min',color=0,/normal,charsize=charsize,charthick=charthick
	py=py-dy & xyouts,x0,py,'Volume of Distribution = '	+PMI__Round(VDone,2)+' ml/100ml'	,color=0,/normal,charsize=charsize,charthick=charthick
	py=py-dy & xyouts,x0,py,'Mean Transit Time = '		+PMI__Round(TTone,2)+' sec'			,color=0,/normal,charsize=charsize,charthick=charthick
	py=py-dy & xyouts,x0,py,'Arterial Delay = '			+PMI__Round(TAone,2)+' sec'			,color=0,/normal,charsize=charsize,charthick=charthick
	py=py-dy & xyouts,x0,py,'Chi-Square fit error = '	+PMI__Round(CSone,3)+' %'			,color=0,/normal,charsize=charsize,charthick=charthick
	py=py-dy
	py=py-dy & xyouts,x0,py,'TWO COMPARTMENTS (PATLAK)'	,color=7*16,/normal,charsize=charsize,charthick=charthick
	py=py-dy & xyouts,x0,py,'Plasma Volume = '			+PMI__Round(PVpat,2)+' ml/100ml'	,color=0,/normal,charsize=charsize,charthick=charthick
	py=py-dy & xyouts,x0,py,'Extraction Flow = '		+PMI__Round(EFpat,2)+' ml/100ml/min',color=0,/normal,charsize=charsize,charthick=charthick
	py=py-dy & xyouts,x0,py,'Arterial Delay = '			+PMI__Round(TApat,2)+' sec'			,color=0,/normal,charsize=charsize,charthick=charthick
	py=py-dy & xyouts,x0,py,'Chi-Square fit error = '	+PMI__Round(CSpat,3)+' %'			,color=0,/normal,charsize=charsize,charthick=charthick
	py=py-dy
	py=py-dy & xyouts,x0,py,'TWO COMPARTMENTS (UPTAKE)'	,color=9*16,/normal,charsize=charsize,charthick=charthick
	py=py-dy & xyouts,x0,py,'Plasma Flow = '		+PMI__Round(PFupt,2)+' ml/100ml/min',color=0,/normal,charsize=charsize,charthick=charthick
	py=py-dy & xyouts,x0,py,'Plasma Volume = '		+PMI__Round(PVupt,2)+' ml/100ml'	,color=0,/normal,charsize=charsize,charthick=charthick
	py=py-dy & xyouts,x0,py,'Plasma MTT = '			+PMI__Round(PTupt,2)+' sec'			,color=0,/normal,charsize=charsize,charthick=charthick
	py=py-dy & xyouts,x0,py,'Extraction Flow = '	+PMI__Round(EFupt,2)+' ml/100ml/min',color=0,/normal,charsize=charsize,charthick=charthick
	py=py-dy & xyouts,x0,py,'Arterial Delay = '		+PMI__Round(TAupt,2)+' sec'			,color=0,/normal,charsize=charsize,charthick=charthick
	py=py-dy & xyouts,x0,py,'Chi-Square fit error = '+PMI__Round(CSupt,3)+' %'			,color=0,/normal,charsize=charsize,charthick=charthick
	py=py-dy
	py=py-dy & xyouts,x0,py,'TWO COMPARTMENTS (TOFTS)'		,color=2*16,/normal,charsize=charsize,charthick=charthick
	py=py-dy & xyouts,x0,py,'Plasma Volume = '		+PMI__Round(PVtof,2)+' ml/100ml'	,color=0,/normal,charsize=charsize,charthick=charthick
	py=py-dy & xyouts,x0,py,'Extraction Flow = '	+PMI__Round(EFtof,2)+' ml/100ml/min',color=0,/normal,charsize=charsize,charthick=charthick
	py=py-dy & xyouts,x0,py,'Interstitial Volume = '+PMI__Round(IVtof,2)+' ml/100ml'	,color=0,/normal,charsize=charsize,charthick=charthick
	py=py-dy & xyouts,x0,py,'Interstitial MTT = '	+PMI__Round(ITtof,2)+' sec'			,color=0,/normal,charsize=charsize,charthick=charthick
	py=py-dy & xyouts,x0,py,'Arterial Delay = '		+PMI__Round(TAtof,2)+' sec'			,color=0,/normal,charsize=charsize,charthick=charthick
	py=py-dy & xyouts,x0,py,'Chi-Square fit error = '+PMI__Round(CStof,3)+' %'			,color=0,/normal,charsize=charsize,charthick=charthick
	py=py-dy
	py=py-dy & xyouts,x0,py,'TWO COMPARTMENTS (EXCHANGE)',color=12*16,/normal,charsize=charsize,charthick=charthick
	py=py-dy & xyouts,x0,py,'Plasma Flow = '		+PMI__Round(PFmam,2)+' ml/100ml/min',color=0,/normal,charsize=charsize,charthick=charthick
	py=py-dy & xyouts,x0,py,'Plasma Volume = '		+PMI__Round(PVmam,2)+' ml/100ml'	,color=0,/normal,charsize=charsize,charthick=charthick
	py=py-dy & xyouts,x0,py,'Plasma MTT = '			+PMI__Round(PTmam,2)+' sec'			,color=0,/normal,charsize=charsize,charthick=charthick
	py=py-dy & xyouts,x0,py,'Extraction Flow = '	+PMI__Round(EFmam,2)+' ml/100ml/min',color=0,/normal,charsize=charsize,charthick=charthick
	py=py-dy & xyouts,x0,py,'Interstitial Volume = '+PMI__Round(IVmam,2)+' ml/100ml'	,color=0,/normal,charsize=charsize,charthick=charthick
	py=py-dy & xyouts,x0,py,'Interstitial MTT = '	+PMI__Round(ITmam,2)+' sec'			,color=0,/normal,charsize=charsize,charthick=charthick
	py=py-dy & xyouts,x0,py,'Arterial Delay = '		+PMI__Round(TAmam,2)+' sec'			,color=0,/normal,charsize=charsize,charthick=charthick
	py=py-dy & xyouts,x0,py,'Chi-Square fit error = '+PMI__Round(CSmam,3)+' %'			,color=0,/normal,charsize=charsize,charthick=charthick

	loadct, 0

	write_tiff, Stdy->DataPath() + RoiName + '_AllProstateModels.tif', reverse(tvrd(/true),3)




	;Write text output




	Pars = make_array(7,12,value='   xxx   ')
	Pars[0,*] = ['Parameter','Pl. Flow ','Pl. Vol. ','Pl. MTT  ','Ext. Flow','Int. Vol.','Int. MTT ','Tot. Vol.','Tot. MTT ','Ext. Frac','Art. Del.','Chi-Sq.  ']
	Pars[1:*,0] = ['Deconv.  ','One-comp.','Patlak   ','Uptake   ','Tofts    ','Mammill. ']

	Pars[1,1] = strnr(PFdec,9)
	Pars[1,7] = strnr(VDdec,9)
	Pars[1,8] = strnr(TTdec,9)
	Pars[1,11] = strnr(CSdec,9)

	Pars[2,1] = strnr(PFone,9)
	Pars[2,7] = strnr(VDone,9)
	Pars[2,8] = strnr(TTone,9)
	Pars[2,10] = strnr(TAone,9)
	Pars[2,11] = strnr(CSone,9)

	Pars[3,2] = strnr(PVpat,9)
	Pars[3,4] = strnr(EFpat,9)
	Pars[3,10] = strnr(TApat,9)
	Pars[3,11] = strnr(CSpat,9)

	Pars[4,1] = strnr(PFupt,9)
	Pars[4,2] = strnr(PVupt,9)
	Pars[4,3] = strnr(PTupt,9)
	Pars[4,4] = strnr(EFupt,9)
	Pars[4,9] = strnr(EXupt,9)
	Pars[4,10] = strnr(TAupt,9)
	Pars[4,11] = strnr(CSupt,9)

	Pars[5,2] = strnr(PVtof,9)
	Pars[5,4] = strnr(EFtof,9)
	Pars[5,5] = strnr(IVtof,9)
	Pars[5,6] = strnr(ITtof,9)
	Pars[5,7] = strnr(PVtof+IVtof,9)
	Pars[5,10] = strnr(TAtof,9)
	Pars[5,11] = strnr(CStof,9)

	Pars[6,1] = strnr(PFmam,9)
	Pars[6,2] = strnr(PVmam,9)
	Pars[6,3] = strnr(PTmam,9)
	Pars[6,4] = strnr(EFmam,9)
	Pars[6,5] = strnr(IVmam,9)
	Pars[6,6] = strnr(ITmam,9)
	Pars[6,7] = strnr(PVmam+IVmam,9)
	Pars[6,8] = strnr(60*(PVmam+IVmam)/PFmam,9)
	Pars[6,9] = strnr(EXmam,9)
	Pars[6,10] = strnr(TAmam,9)
	Pars[6,11] = strnr(CSmam,9)

	Pars = Pars + '	'

	openw, 1, Stdy->DataPath() + RoiName + '_AllProstateModels.txt'
	printf, 1, Pars
	close, 1

	return: PMI__Message, status
end

function PMI__Button__ProstateRoiModels, parent,value=value, separator=separator

	if n_elements(value) eq 0 then value = 'Fit prostate models (ROI)'

	id = widget_button(parent $
	,	value 		= value $
	,	event_pro 	= 'PMI__Button__ProstateRoiModels__event'$
	, 	separator 	= separator	)

	return, id

end
