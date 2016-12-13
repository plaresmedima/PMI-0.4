function PMI__Button__MalihaROIfit__input $
, 	ev $
,	Status	= id $
, 	Stdy	= Stdy $
,	time 	= time $
,	curve 	= roicurve $
,	aif		= aif $
,	Roi		= Roi $
,	vol 	= vol

	PMI__Info, ev.top, Status=id, Stdy=Stdy

   Series = Stdy->Names(0,ns,DefDim=3,ind=ind,sel=sel)
    if ns eq 0 then begin
       ok = dialog_message(/information,'Please load a dynamic series')
       return, 0
    endif
    Regions = Stdy->names(1,nr)
    if nr eq 0 then begin
       ok = dialog_message(/information,'No arterial or tissue regions are defined')
       return, 0
    endif

	in = cw_InputForm(/pos, ev=ev,										$
		Title 	= 'ROI analysis' $
	,	Labels 	= 											$
		[	'Dynamic series' 								$
		,	'Arterial region'								$
		,	'Region of interest'							$
		,	'Length of baseline (# of dynamics)'			$
		, 	'Last time point (sec)'$
		]													$
	,	ListNames 		= [ Series,Regions,Regions]$
	,	ListNumbers 	= [	ns,nr,nr]$
	,	ListDefaults 	= [	sel,Stdy->sel(1),Stdy->sel(1)]$
	,	DataDefaults 	= {	nb:10L, t1:90E}$
	) & if size(in,/type) eq 1 then return, 0

	Series = Stdy->Obj(0,ind[in.select[0]])
	Art = Stdy->Obj(1,in.select[1])
	Roi = Stdy->Obj(1,in.select[2])

    Time = Series->c(1)
    Time = Time-Time[0]
	nt = total(time lt in.data.t1)
    if nt eq 0 then begin
    	ok = dialog_message(/information,'No data at these times')
    	return, 0
    end
	time = time[0:nt-1]


	; GET AIF


	Aif = PMI__RoiCurve(Stdy->DataPath(), Series, Art, id, cnt=cnt)
	if cnt eq 0 then begin
       ok = dialog_message(/information,'Empty arterial region!')
       return, 0
    endif
	Aif = LMU__Enhancement(Aif,in.data.nb,relative=1)/(1-0.45)
	aif = aif[0:nt-1]

  	cw_create_plot, Aif $
  	,	xaxis 	= Time $
	,	xtitle 	= 'Time (/sec)' $
	,	ytitle 	= 'Relative Enhancement' $
	,	title 	= 'Arterial Input Function'


	; GET ROICURVE


	RoiCurve = PMI__RoiCurve(Stdy->DataPath(), Series, Roi, id, cnt=cnt)
	if cnt eq 0 then begin
       ok = dialog_message(/information,'Empty ROI!')
       return, 0
    endif
	RoiCurve = LMU__Enhancement(RoiCurve,in.data.nb,relative=1)
	roicurve = roicurve[0:nt-1]

	;ROI volume

	v = PMI__RoiValues(Stdy->DataPath(), Series,Roi,status,cnt=npix)
	vol = 0.048828125^2*0.25*npix

	return, 1
end



pro PMI__Button__MalihaROIfit__output $
   ,	Path $
   , 	time $
   , 	roicurve $
   , 	Fit $
   , 	RoiName $
   , 	PV=PV,PF=PF,EF=EF,PT=PT,IT=IT,Extr=Extr, vol=vol, GFR=GFR

  window, xsize=1200, ysize=600

  n = n_elements(roicurve)
  MinCurve = min(roicurve,max=MaxCurve)
  MinFit = min(Fit,max=MaxFit)

  plot, /nodata, position=[0.1,0.2,0.5,0.9]  $
        ,	Title = RoiName $
        , 	[0,time[n-1]], [min([MinCurve,MinFit]),max([MaxCurve,MaxFit])] $
        , 	/xstyle, /ystyle $
        , 	background=255, color=0 $
        , 	xtitle = 'Time (sec)', ytitle='Relative Enhancement' $
        , 	charsize=2.0, charthick=2.0, xthick=2.0, ythick=2.0

  loadct, 12,/silent

  oplot, Time, roicurve	, color=6*16	, linestyle = 0, thick=2
  oplot, Time, Fit		, color=12*16	, linestyle = 0, thick=2

  top=0.9 & dy=0.05 & x0=0.525 & charsize=2.0 & charthick=2.0

  xyouts,x0,top-0*dy,'2-compartment Filtration model', color=0,/normal,charsize=charsize,charthick=charthick

  if n_elements(PF) ne 0 then xyouts,x0,top-2*dy,'Plasma Flow = '			+PMI__Round(PF,0,/string)+' ml/min/100ml',color=0,/normal,charsize=charsize,charthick=charthick
  if n_elements(PV) ne 0 then xyouts,x0,top-3*dy,'Plasma Volume = '			+PMI__Round(PV,1,/string)+' ml/100ml'	,color=0,/normal,charsize=charsize,charthick=charthick
  if n_elements(PT) ne 0 then xyouts,x0,top-4*dy,'Plasma MTT = '			+PMI__Round(PT,1,/string)+' sec'			,color=0,/normal,charsize=charsize,charthick=charthick
  if n_elements(EF) ne 0 then xyouts,x0,top-6*dy,'Tubular Flow = '		+PMI__Round(EF,1,/string)+' ml/min/100ml',color=0,/normal,charsize=charsize,charthick=charthick
  if n_elements(IT) ne 0 then xyouts,x0,top-7*dy,'Tubular MTT = '		+PMI__Round(IT,0,/string)+' sec'			,color=0,/normal,charsize=charsize,charthick=charthick
  if n_elements(Extr) ne 0 then xyouts,x0,top-9*dy,'Extraction Fraction = '	+PMI__Round(Extr,1,/string) +' %'		,color=0,/normal,charsize=charsize,charthick=charthick
	xyouts,x0,top-11*dy,'ROI volume = '	+PMI__Round(vol,2,/string) +' ml'		,color=0,/normal,charsize=charsize,charthick=charthick
	xyouts,x0,top-12*dy,'ROI GFR = '	+PMI__Round(GFR,2,/string) +' ml/min'		,color=0,/normal,charsize=charsize,charthick=charthick

  loadct, 0, /silent

  write_tiff, path + Roiname + '_FiltrationDelay.tif', reverse(tvrd(/true),3),compression =1
  PMI__WritePlot, path + Roiname +'_Curve.txt', Time, roicurve
  PMI__WritePlot, path + Roiname +'_FiltrationDelay_fit.txt', Time, Fit
end





pro PMI__Button__MalihaROIfit__event, ev

  if not PMI__Button__MalihaROIfit__input($
     ev $
     ,	Status	= status $
     , 	Stdy	= Stdy $
     ,	time 	= time $
     ,	curve 	= roicurve $
     ,	aif		= aif $
     ,	Roi		= Roi $
     ,	vol		= vol $
  ) then goto, return

  PMI__Message, Status, 'Fitting..'

  n=n_elements(RoiCurve)
  Fit = TwoCompartmentDelayFit_Indirect(Time,RoiCurve,Aif,0,Init=5,/nodelay,/mp,/noderivative,/quiet,/constrained)
  AIC_2CFM = n*alog(total((RoiCurve-Fit)^2)/n) + 2D*(1+4)
  Fit = FitSingleInletUptakeDelay(Time,RoiCurve,Aif,Init=5,/nodelay,/mp,/noderivative,/quiet,/constrained)
  AIC_2CUM = n*alog(total((RoiCurve-Fit)^2)/n) + 2D*(1+3)
  Fit = FitToftsDelay(Time,RoiCurve,Aif,Init=5,/nodelay,/mp,/noderivative,/quiet,/constrained)
  AIC_2CTM = n*alog(total((RoiCurve-Fit)^2)/n) + 2D*(1+3)
  Fit = FitKetyDel(Time,RoiCurve,Aif,Init=5,/nodelay,/mp,/noderivative,/quiet,/constrained)
  AIC_1CM = n*alog(total((RoiCurve-Fit)^2)/n) + 2D*(1+2)

  minAIC = min([AIC_2CFM, AIC_2CUM, AIC_2CTM, AIC_1CM], i)

  CASE i OF
  	0:BEGIN
  		Fit = TwoCompartmentDelayFit_Indirect(Time,RoiCurve,Aif,0,Init=5,Kinetic=Pk,Convective=Pc,/nodelay,/mp,/noderivative,/quiet,/constrained)
       	PV = 100.0*Pc[2]
     	PF = 6000.0*Pc[4]
     	EF = 6000.0*Pc[0]
     	PT = Pk[2]
     	IT = Pk[3]
     	Extr = 100.0*Pc[0]/Pc[4]
  	END
  	1:BEGIN
  		Fit = FitSingleInletUptakeDelay(time,roicurve,aif,Pars=Pk,Init=5,Convective=Pc,/nodelay,/mp,/noderivative,/quiet,/constrained)
       	PV = 100D*Pc[1]
     	PF = 6000D*Pc[2]
     	EF = 6000D*Pc[0]
     	PT = 1D*Pk[1]
     	Extr = 100D*Pc[0]/Pc[2]
  	END
  	2:BEGIN
  		Fit = FitToftsDelay(time,roicurve,aif,Pars=P,Convective=Pc,Init=5,/nodelay,/mp,/noderivative,/quiet,/constrained)
	    PV = 100D*Pc[1]
     	EF = 6000D*Pc[0]
     	IT = 1D/P[2]
  	END
  	3:BEGIN
  		Fit = FitKetyDel(time,roicurve,aif,Pars=P,Init=5,/nodelay,/mp,/noderivative,/quiet,/constrained)
     	EF = 6000D*P[0]
     	IT = 1D*P[1]/P[0]
  	END
  ENDCASE

  PMI__Button__MalihaROIfit__output $
     ,	Stdy->DataPath() $
     ,	time $
     ,	roicurve $
     ,	Fit $
     , 	Roi->Name() $
     ,	PV = PV $
     ,	PF = PF $
     ,	EF = EF $
     ,	PT = PT $
     ,	IT = IT $
     ,	Extr = Extr $
     ,	vol = vol $
     ,	GFR = vol*EF/100

  return: PMI__Message, status
end


pro PMI__Button__Control__MalihaROIfit, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 1)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__MalihaROIfit, parent,value=value, separator=separator

	if n_elements(value) eq 0 then value = 'Fit filtration model (ROI)'

	id = widget_button(parent $
	,	value 		= value	$
	,	event_pro 	= 'PMI__Button__MalihaROIfit__event' $
	,	pro_set_value 	= 'PMI__Button__Control__MalihaROIfit' $
	, 	separator 	= separator	)

	return, id
end
