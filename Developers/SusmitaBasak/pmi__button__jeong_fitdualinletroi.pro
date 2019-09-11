;
;;

PRO PMI__Button__Display__Jeong_FitDualInletRoi::Fit

	Self->GET, Model=Model, AIF_Delay=AIF_Delay, VIF_Delay=VIF_Delay, AIF_Use=AIF_Use, VIF_Use=VIF_Use, Time=Time, AifCurve=Aif, VifCurve=Vif, RoiCurve=Curve, OnDisplay=OnDisplay
	Self->SET, Message='Fitting...', Sensitive=0

	DELAY_VALUES=[0,20,1]

	IF keyword_set(AIF_Delay) AND keyword_set(VIF_Delay) THEN DELAY_WHICH=2 $
	ELSE IF keyword_set(AIF_Delay) THEN DELAY_WHICH=0 $
	ELSE IF keyword_set(VIF_Delay) THEN DELAY_WHICH=1 $
	ELSE DELAY_WHICH=-1

	FA = 0.2*50/6000D
	FV = 0.8*50/6000D
	IF NOT keyword_set(AIF_Use) THEN BEGIN
		FA = 0
		Fixed = 0
		IF DELAY_WHICH EQ 2 THEN DELAY_WHICH=1
		IF DELAY_WHICH EQ 0 THEN DELAY_WHICH=-1
	ENDIF
	IF NOT keyword_set(VIF_Use) THEN BEGIN
		FV = 0
		IF (Model EQ 'Patlak') OR (Model EQ 'Modified Tofts') THEN Fixed=0 ELSE Fixed=1
		IF DELAY_WHICH EQ 2 THEN DELAY_WHICH=0
		IF DELAY_WHICH EQ 1 THEN DELAY_WHICH=-1
	ENDIF
	AF = FA/(FA+FV)


	CASE Model OF

		'Compartment':BEGIN
			P = [FA,FV,0.2] ;P = [FA,FV,VE+VP]
			Fit = FitDualInlet('Compartment', Time, Aif, Vif, Curve, P, LIMITED_ABOVE=[0,0,1], DELAY_PAR=Pd, DELAY_VALUES=DELAY_VALUES, DELAY_WHICH=DELAY_WHICH, AKAIKE_ERROR=aic, /POSITIVITY, /NODERIVATIVE, FIXED=fixed)
			Parameters = $
				[{Name:'Extracellular Volume'	,Units:'ml/100ml'		,Value:100D*P[2]				,Nr: 4} $
	;			,{Name:'Akaike Fit Error'		,Units:''				,Value:AIC						,Nr:16} $
				,{Name:'Extracellular MTT'		,Units:'sec'		,Value:1D*P[2]/(P[0]+P[1])		,Nr: 3} ]
			IF keyword_set(AIF_Use) THEN 	Parameters = [Parameters $
				,{Name:'Arterial Blood Flow'	,Units:'ml/100ml/min'	,Value:6000*P[0]/(1-self.Hematocrit)				,Nr: 0} ]
			IF keyword_set(VIF_Use) THEN 	Parameters = [Parameters $
				,{Name:'Venous Blood Flow'		,Units:'ml/100ml/min'	,Value:6000*P[1]/(1-self.Hematocrit)				,Nr: 1} ]
			IF keyword_set(AIF_Use) AND keyword_set(VIF_Use) THEN 	Parameters = [Parameters $
				,{Name:'Arterial Flow Fraction'	,Units:'%' 				,Value:100*P[0]/(P[0]+P[1])		,Nr: 2} $
				,{Name:'Total Blood Flow'	 	,Units:'ml/100ml/min' 	,Value:6000D*(P[0]+P[1])/(1-self.Hematocrit)		,Nr: -1} ]
			END

		'Patlak':BEGIN
			P = [AF,0.2,0.2*(FA+FV)] ;P = [AF,EV,KI]
			Fit = FitDualInlet('Patlak', Time, Aif, Vif, Curve, P, LIMITED_ABOVE=[1,1,0], DELAY_PAR=Pd, DELAY_VALUES=DELAY_VALUES, DELAY_WHICH=DELAY_WHICH, AKAIKE_ERROR=aic, /POSITIVITY, /NODERIVATIVE, FIXED=fixed)
			Parameters = $
				[{Name:'Extracellular Volume'	,Units:'ml/100ml'		,Value:100D*P[1]		,Nr: 3} $
				,{Name:'Akaike Fit Error'		,Units:''				,Value:AIC				,Nr:16} $
				,{Name:'Uptake Rate'			,Units:'/100/min'		,Value:6000D*P[2]		,Nr:12} ]
			IF keyword_set(AIF_Use) AND keyword_set(VIF_Use) THEN 	Parameters = [Parameters $
				,{Name:'Arterial Flow Fraction'	,Units:'%' 				,Value:100D*P[0]			,Nr: 2} ]
			END

		'Modified Tofts':BEGIN
			P = [AF, 0.9, 0.7, 0.2*(FA+FV)] ;P = [AF, VE+VI, VI/(VE+VI), KI]
			Fit = FitDualInlet('ModTofts', Time, Aif, Vif, Curve, P, LIMITED_ABOVE=[1,1,1,0], DELAY_PAR=Pd, DELAY_VALUES=DELAY_VALUES, DELAY_WHICH=DELAY_WHICH, AKAIKE_ERROR=aic, /POSITIVITY, /NODERIVATIVE, FIXED=fixed)
			Parameters = $
				[{Name:'Extracellular Volume'	,Units:'ml/100ml'		,Value:100D*P[1]*(1-P[2])		,Nr: 3} $
				,{Name:'Akaike Fit Error'		,Units:''				,Value:AIC				,Nr:16} $
				,{Name:'Uptake Rate'			,Units:'/100/min'		,Value:6000D*P[3]		,Nr:12} $
				,{Name:'Intracellular Volume'	,Units:'ml/100ml'		,Value:100D*P[1]*P[2]		,Nr: 8} $
				,{Name:'Intracellular MTT'		,Units:'sec'		,Value:1D*P[1]*(1-P[2])/P[3]		,Nr: 7}]
			IF keyword_set(AIF_Use) AND keyword_set(VIF_Use) THEN 	Parameters = [Parameters $
				,{Name:'Arterial Flow Fraction'	,Units:'%' 				,Value:100D*P[0]			,Nr: 2} ]
			END

		'Uptake':BEGIN
			P = [FA,FV,0.2,0.2] ;P = [FA,FV,VE,EI]
			Fit = FitDualInlet('Uptake', Time, Aif, Vif, Curve, P, LIMITED_ABOVE=[1,1,1,1], LIMITS_ABOVE=[1000/6000D,1000/6000D,1,1D], $
			  DELAY_PAR=Pd, DELAY_VALUES=DELAY_VALUES, DELAY_WHICH=DELAY_WHICH, AKAIKE_ERROR=aic, /POSITIVITY, /NODERIVATIVE, FIXED=fixed)
			Parameters = $
				[{Name:'Extracellular Volume'	,Units:'ml/100ml'		,Value:100D*P[2]				,Nr: 4} $
				,{Name:'Akaike Fit Error'		,Units:''				,Value:AIC						,Nr:16} $
				,{Name:'Extracellular MTT'		,Units:'sec'			,Value:1D*P[2]*(1-P[3])/(P[0]+P[1])			,Nr: 3} $
				,{Name:'Uptake Fraction'		,Units:'%' 				,Value:100D*P[3]							,Nr:11} $
				,{Name:'Uptake Rate'			,Units:'/100/min'		,Value:6000D*(P[0]+P[1])*P[3]/(1-P[3])		,Nr:12} ]
			IF keyword_set(AIF_Use) THEN 	Parameters = [Parameters $
				,{Name:'Arterial Plasma Flow'	,Units:'ml/100ml/min'	,Value:6000*P[0]				,Nr: 0} ]
			IF keyword_set(VIF_Use) THEN 	Parameters = [Parameters $
				,{Name:'Venous Plasma Flow'		,Units:'ml/100ml/min'	,Value:6000*P[1]				,Nr: 1} ]
			IF keyword_set(AIF_Use) AND keyword_set(VIF_Use) THEN 	Parameters = [Parameters $
				,{Name:'Arterial Flow Fraction'	,Units:'%' 				,Value:100*P[0]/(P[0]+P[1])		,Nr: 2} $
				,{Name:'Total Plasma Flow'	 	,Units:'ml/100ml/min' 	,Value:6000D*(P[0]+P[1])		,Nr: -1}]
			END

		'Filtration':BEGIN
			P = [FA,FV,0.2,0.2,0.2] ;P = [FA,FV,VE,EI,EB]
			Fit = FitDualInlet('Filtration', Time, Aif, Vif, Curve, P, LIMITED_ABOVE=[1,1,1,1,1], LIMITS_ABOVE=[1000/6000D,1000/6000D,1,1D,1], $
			  DELAY_PAR=Pd, DELAY_VALUES=DELAY_VALUES, DELAY_WHICH=DELAY_WHICH, AKAIKE_ERROR=aic, /POSITIVITY, /NODERIVATIVE, FIXED=fixed)
			Parameters = $
				[{Name:'Extracellular Volume'	,Units:'ml/100ml'		,Value:100D*P[2]				,Nr: 4} $
				,{Name:'Akaike Fit Error'		,Units:''				,Value:AIC						,Nr:16} $
				,{Name:'Extracellular MTT'		,Units:'sec'			,Value:1D*P[2]*(1-P[3])/(P[0]+P[1])			,Nr: 3} $
				,{Name:'Uptake Fraction'		,Units:'%' 				,Value:100D*P[3]							,Nr:11} $
				,{Name:'Uptake Rate'			,Units:'/100/min'		,Value:6000D*(P[0]+P[1])*P[3]/(1-P[3])		,Nr:12} $
				,{Name:'Excretion Rate'			,Units:'/100/min'		,Value:6000D*(P[0]+P[1])*P[4]/(1-P[4])		,Nr:13} ]
			IF keyword_set(AIF_Use) THEN 	Parameters = [Parameters $
				,{Name:'Arterial Plasma Flow'	,Units:'ml/100ml/min'	,Value:6000*P[0]				,Nr: 0} ]
			IF keyword_set(VIF_Use) THEN 	Parameters = [Parameters $
				,{Name:'Venous Plasma Flow'		,Units:'ml/100ml/min'	,Value:6000*P[1]				,Nr: 1} ]
			IF keyword_set(AIF_Use) AND keyword_set(VIF_Use) THEN 	Parameters = [Parameters $
				,{Name:'Arterial Flow Fraction'	,Units:'%' 				,Value:100*P[0]/(P[0]+P[1])		,Nr: 2} $
				,{Name:'Total Plasma Flow'	 	,Units:'ml/100ml/min' 	,Value:6000D*(P[0]+P[1])		,Nr: -1}]
			END

		'Exchange':BEGIN
			P = [FA,FV,0.2,0.2,0.5] ;P = [FA,FV,VE+VP,EX,VP/(VE+VP)]
			Fit = FitDualInlet('Exchange', Time, Aif, Vif, Curve, P, LIMITED_ABOVE=[0,0,1,1,1], DELAY_PAR=Pd, DELAY_VALUES=DELAY_VALUES, DELAY_WHICH=DELAY_WHICH, AKAIKE_ERROR=aic, /POSITIVITY, /NODERIVATIVE, FIXED=fixed)
			Parameters = $
				[{Name:'Extracellular Volume'	,Units:'ml/100ml'		,Value:100D*P[2]				,Nr: 4} $
				,{Name:'Akaike Fit Error'		,Units:''				,Value:AIC						,Nr:16} $
				,{Name:'Extracellular MTT'		,Units:'sec'			,Value:1D*P[2]/(P[0]+P[1])		,Nr: 3} $
				,{Name:'Plasma MTT'				,Units:'sec'			,Value:1D*P[2]*P[4]/((P[0]+P[1])/(1-P[3]))	,Nr: 5} $
				,{Name:'Plasma Volume'			,Units:'ml/100ml'		,Value:100D*P[2]*P[4]						,Nr: 6} $
				,{Name:'Interstitial MTT'		,Units:'sec'			,Value:1D*P[2]*(1-P[4])/((P[0]+P[1])*P[3]/(1-P[3]))						,Nr: 7} $
				,{Name:'Interstitial Volume'	,Units:'ml/100ml'		,Value:100D*P[2]*(1-P[4])						,Nr: 8} $
				,{Name:'Exchange Fraction'		,Units:'%' 				,Value:100D*P[3]		,Nr: 9} $
				,{Name:'Extraction Flow'		,Units:'ml/100ml/min'	,Value:6000D*(P[0]+P[1])*P[3]/(1-P[3])		,Nr: 10} ]
			IF keyword_set(AIF_Use) THEN 	Parameters = [Parameters $
				,{Name:'Arterial Plasma Flow'	,Units:'ml/100ml/min'	,Value:6000*P[0]				,Nr: 0} ]
			IF keyword_set(VIF_Use) THEN 	Parameters = [Parameters $
				,{Name:'Venous Plasma Flow'		,Units:'ml/100ml/min'	,Value:6000*P[1]				,Nr: 1} ]
			IF keyword_set(AIF_Use) AND keyword_set(VIF_Use) THEN 	Parameters = [Parameters $
				,{Name:'Arterial Flow Fraction'	,Units:'%' 				,Value:100*P[0]/(P[0]+P[1])		,Nr: 2} $
				,{Name:'Total Plasma Flow'	 	,Units:'ml/100ml/min' 	,Value:6000D*(P[0]+P[1])		,Nr: -1}]
			END

		'Exchange-Uptake': BEGIN
			P = [FA,FV,0.2,0.2,0.5,0.2] ;P = [FA,FV,VE+VP,EX,VP/(VE+VP),EI]
			Fit = FitDualInlet('ExchangeUptake', Time, Aif, Vif, Curve, P, LIMITED_ABOVE=[0,0,1,1,1,1], DELAY_PAR=Pd, DELAY_VALUES=DELAY_VALUES, DELAY_WHICH=DELAY_WHICH, AKAIKE_ERROR=aic, /POSITIVITY, /NODERIVATIVE, FIXED=fixed)
			Parameters = $
				[{Name:'Extracellular Volume'	,Units:'ml/100ml'		,Value:100D*P[2]				,Nr: 4} $
				,{Name:'Akaike Fit Error'		,Units:''				,Value:AIC						,Nr:16} $
				,{Name:'Extracellular MTT'		,Units:'sec'			,Value:1D*P[2]/(P[0]+P[1])		,Nr: 3} $
				,{Name:'Plasma MTT'				,Units:'sec'			,Value:1D*P[2]*P[4]/((P[0]+P[1])/(1-P[3]))	,Nr: 5} $
				,{Name:'Plasma Volume'			,Units:'ml/100ml'		,Value:100D*P[2]*P[4]						,Nr: 6} $
				,{Name:'Interstitial MTT'		,Units:'sec'			,Value:1D*P[2]*(1-P[4])/((P[0]+P[1])*P[3]/(1-P[3]))		,Nr: 7} $
				,{Name:'Interstitial Volume'	,Units:'ml/100ml'		,Value:100D*P[2]*(1-P[4])		,Nr: 8} $
				,{Name:'Exchange Fraction'		,Units:'%' 				,Value:100D*P[3]				,Nr: 9} $
				,{Name:'Extraction Flow'		,Units:'ml/100ml/min'	,Value:6000D*(P[0]+P[1])*P[3]/(1-P[3])		,Nr: 10} $
				,{Name:'Uptake Fraction'		,Units:'%' 				,Value:100D*P[5]			,Nr: 11} $
				,{Name:'Uptake Rate'			,Units:'/100/min'		,Value:6000D*(P[0]+P[1])*(P[3]/(1-P[3]))*P[5]/(1-P[5])	,Nr: 12} ]
			IF keyword_set(AIF_Use) THEN 	Parameters = [Parameters $
				,{Name:'Arterial Plasma Flow'	,Units:'ml/100ml/min'	,Value:6000*P[0]				,Nr: 0} ]
			IF keyword_set(VIF_Use) THEN 	Parameters = [Parameters $
				,{Name:'Venous Plasma Flow'		,Units:'ml/100ml/min'	,Value:6000*P[1]				,Nr: 1} ]
			IF keyword_set(AIF_Use) AND keyword_set(VIF_Use) THEN 	Parameters = [Parameters $
				,{Name:'Arterial Flow Fraction'	,Units:'%' 				,Value:100*P[0]/(P[0]+P[1])		,Nr: 2} $
				,{Name:'Total Plasma Flow'	 	,Units:'ml/100ml/min' 	,Value:6000D*(P[0]+P[1])		,Nr: -1}]
			END
	ENDCASE

	IF DELAY_WHICH EQ 2 THEN Parameters = [Parameters $
		,{Name:'Arterial Delay'	,Units:'sec',Value:1D*Pd[0]	,Nr:14} $
		,{Name:'Venous Delay'	,Units:'sec',Value:1D*Pd[1]	,Nr:15} ] $
	ELSE IF DELAY_WHICH EQ 0 THEN Parameters = [Parameters $
		,{Name:'Arterial Delay'	,Units:'sec',Value:1D*Pd	,Nr:14} ] $
	ELSE IF DELAY_WHICH EQ 1 THEN Parameters = [Parameters $
		,{Name:'Venous Delay'	,Units:'sec',Value:1D*Pd	,Nr:15} ]

	self.Curve[3] = ptr_new(Fit)
	self.Parameters = ptr_new(Parameters)
	Self->SET, OnDisplay=OnDisplay, /Sensitive
END









PRO PMI__Button__Display__Jeong_FitDualInletRoi::Plot

	Self->GET, OnDisplay=OnDisplay

	top=0.9 & dy=0.04 & x0=0.525 & charsize=1.0 & charthick=1.0

	case self.units of
	'Linear (a.u.)': ytitle = 'Signal Enhancement (a.u.)'
	'Linear (%)': ytitle = 'Relative Signal Enhancement (%)'
	'Linear (mM)': ytitle = 'Concentration (mM)'
	'Non-linear (mM)': ytitle = 'Concentration (mM)'
	endcase


	CASE OnDisplay OF

		'':Self->Set, /Erase

		'ROI':begin
			Self -> GET, Time=Time, RoiCurve=Y, RoiName=RoiName
			Self -> SET, /Erase
 			plot, /nodata, position=[0.1,0.2,0.5,0.9]  $
			, 	[0,max(time)], [min(Y),max(Y)] $
			, 	/xstyle, /ystyle $
			, 	background=255, color=0 $
			, 	xtitle = 'Time (sec)', ytitle=ytitle $
			, 	charsize=1.5, charthick=2.0, xthick=2.0, ythick=2.0
			oplot, time, Y, color=6*16, linestyle=0, thick=2
			xyouts, x0, top-0*dy, 'Region Of Interest: ' + RoiName, color=6*16, /normal, charsize=1.5, charthick=1.5
			end

		'AIF':begin
			Self -> GET, Time=Time, AifCurve=Y, AifName=AifName
			Self -> SET, /Erase
 			plot, time, Y, position=[0.1,0.2,0.5,0.9]  $
			, 	/xstyle, /ystyle $
			, 	background=255, color=0 $
			, 	xtitle = 'Time (sec)', ytitle=ytitle $
			, 	linestyle=0, thick=2 $
			, 	charsize=1.5, charthick=2.0, xthick=2.0, ythick=2.0
			xyouts, x0, top-1*dy, 'Arterial Input Function: ' + AifName	, color=0, /normal, charsize=1.5, charthick=1.5
			end

		'VIF':begin
			Self -> GET, Time=Time, VifCurve=Y, VifName=VifName
			Self -> SET, /Erase
 			plot, time, Y, position=[0.1,0.2,0.5,0.9]  $
			, 	/xstyle, /ystyle $
			, 	background=255, color=0 $
			, 	xtitle = 'Time (sec)', ytitle=ytitle $
			, 	linestyle=0, thick=2 $
			, 	charsize=1.5, charthick=2.0, xthick=2.0, ythick=2.0
			xyouts, x0, top-2*dy, 'Venous Input Function: ' + VifName	, color=0, /normal, charsize=1.5, charthick=1.5
			end

		'FIT':BEGIN
			Self -> GET, RoiCurve=Curve, Time=Time, Fit=Fit, Model=Model, RoiName=RoiName, AifName=AifName, VifName=VifName
			Self -> SET, /Erase

 			plot, /nodata, position=[0.1,0.2,0.5,0.9]  $
			, 	[0,max(time)], [min([min(Curve),min(Fit)]),max([max(Curve),max(Fit)])] $
			, 	/xstyle, /ystyle $
			, 	background=255, color=0 $
			, 	xtitle = 'Time (sec)', ytitle=ytitle $
			, 	charsize=1.5, charthick=2.0, xthick=2.0, ythick=2.0
			oplot, time, Curve, color=6*16, linestyle=0, thick=2
			oplot, time, Fit, color=12*16, linestyle=0, thick=2

			xyouts, x0, top-0*dy, 'Region Of Interest: ' + RoiName		, color=6*16, /normal, charsize=1.5, charthick=1.5
			xyouts, x0, top-1*dy, 'Arterial Input Function: ' + AifName	, color=0, /normal, charsize=1.5, charthick=1.5
			xyouts, x0, top-2*dy, 'Venous Input Function: ' + VifName	, color=0, /normal, charsize=1.5, charthick=1.5
			xyouts, x0, top-3*dy, 'Tissue Model: ' + Model				, color=12*16, /normal, charsize=1.5, charthick=1.5

			P = *self.Parameters

			for i=0L,n_elements(P)-1 do xyouts $
				, x0, top-dy*(5+P[i].Nr) $
				, P[i].Name + ' = ' + PMI__Round(P[i].Value,2,/string) + ' ' + P[i].Units $
				, color=0, /normal, charsize=charsize, charthick=charthick
			END
	ENDCASE

END






FUNCTION PMI__Button__Display__Jeong_FitDualInletRoi::Event, ev

	Uname = widget_info(ev.id,/uname)

	i = where(Uname Eq ['ROI','AIF','VIF','FIT']+'bttn', cnt)
	If cnt eq 1 then begin
		Self->SET, OnDisplay=strmid(Uname,0,3), /Refresh
		return, 0B
	endif

	i = where(Uname Eq ['ROI','AIF','VIF'], cnt)
	If cnt eq 1 then begin
		ptr_free, Self.Curve[i], Self.Curve[3], self.parameters
		Self->SET, OnDisplay=Uname, /Refresh
		return, 0B
	endif


	i = where(Uname Eq ['FIT','AIF Delay','VIF Delay', 'AIF Use','VIF Use'], cnt)
	if cnt eq 1 then begin
		ptr_free, Self.Curve[3], self.parameters
		Self->SET, OnDisplay='ROI', /Refresh
		i = where(Uname Eq 'AIF Use', cnt)
		if cnt eq 1 then begin
			set = widget_info(widget_info(self.id, find_by_uname='AIF Use'), /button_set)
			widget_control, widget_info(self.id, find_by_uname='AIFbttn'), sensitive=set
			widget_control, widget_info(self.id, find_by_uname='AIF'), sensitive=set
			widget_control, widget_info(self.id, find_by_uname='AIFbaseDel'), sensitive=set
			widget_control, widget_info(self.id, find_by_uname='VIFbaseUse'), sensitive=set
		endif
		i = where(Uname Eq 'VIF Use', cnt)
		if cnt eq 1 then begin
			set = widget_info(widget_info(self.id, find_by_uname='VIF Use'), /button_set)
			widget_control, widget_info(self.id, find_by_uname='VIFbttn'), sensitive=set
			widget_control, widget_info(self.id, find_by_uname='VIF'), sensitive=set
			widget_control, widget_info(self.id, find_by_uname='VIFbaseDel'), sensitive=set
			widget_control, widget_info(self.id, find_by_uname='AIFbaseUse'), sensitive=set
		endif

		return, 0B
	endif



	i = where(Uname Eq ['Export','Export As'], cnt)
	if cnt eq 1 then begin
		Self->GET, Time=Time, RoiCurve=RoiCurve, Fit=Fit, Model=Model, Roiname=Roiname
		if Uname eq 'Export' then begin
			PMI__Info, ev.top, Stdy=Stdy
			Path = Stdy->Datapath() + 'Dual-Inlet Models (ROI)'
			file_mkdir, Path
			File = Path + '\' + Roiname + '__DI_' + Model
		endif else begin
			PMI__Info, ev.top, State=State
			if not State -> Get_file(file, file=cleanstr(Roiname + '__' + Model), title='Save as..', filter='.tif') then return,0B
			File = strmid(file,0,strlen(file)-4)
		endelse
		Write_tiff, File + '.tif', reverse(tvrd(/true),3)
		PMI__WritePlot, File + '__Curve.txt', Time, RoiCurve
		PMI__WritePlot, File + '__Fit.txt', time, Fit
		PMI__WritePlot, File + '__Parameters.txt' $
		, 	Xname = 'Parameters', (*self.Parameters).Name + '  (' + (*self.Parameters).Units + ')' $
		, 	Yname = 'Fitted Values', (*self.Parameters).Value
		return, 0B
	endif


	Menu = widget_info(widget_info(ev.top,/child),/all_children)
	for i=0L,n_elements(Menu)-1 do widget_control, Menu[i], /sensitive
	PMI__Control, ev.top, Viewer = 'PMI__Display__2DVIEW'
	PMI__Control, ev.top, /refresh
	return, 0B
END

FUNCTION PMI__Button__Display__Event__Jeong_FitDualInletRoi, ev

	widget_control, ev.handler, get_uvalue=self
	return, Self -> Event(ev)
END



FUNCTION PMI__Button__Display__Jeong_FitDualInletRoi::GetCurve, List

	PMI__Info, tlb(self.id), Stdy=Stdy
	Region = Stdy->Obj(1,widget_info(widget_info(self.id,find_by_uname=List),/droplist_select))
	Self -> GET, OnDisplay=OnDisplay, Time=Time
	Self -> SET, Message = 'Loading ' + Region->Name() + ' curve', Sensitive=0
	Y = PMI__RoiCurve(Stdy->DataPath(), Self.Series, Region, cnt=cnt)
	Self -> SET, OnDisplay=OnDisplay, /Sensitive
	if cnt eq 0 then return, Time*0

	S0 = total(Y[0:self.Baseline-1])/self.Baseline
	case self.units of
	'Linear (a.u.)': return, Y-S0
	'Linear (%)': return, 100E*(Y-S0)/S0
	'Linear (mM)': BEGIN
		CASE List OF
			'ROI':R10=1/self.T1t
			'AIF':R10=1/self.T1a
			'VIF':R10=1/self.T1v
		ENDCASE
		return, (1000*R10/self.relaxivity)*(Y-S0)/S0
		END
	'Non-linear (mM)': BEGIN
		S0 = total(Y[0:self.Baseline-1])/self.Baseline
		CASE List OF
			'ROI':T10=self.T1t
			'AIF':T10=self.T1a
			'VIF':T10=self.T1v
		ENDCASE
		return, Concentration_SPGRESS(Y, S0, T10, self.FA, self.TR, self.Relaxivity)
		END
	endcase


END

FUNCTION PMI__Button__Display__Jeong_FitDualInletRoi::GetName, List

	PMI__Info, tlb(self.id), Stdy=Stdy
	Region = Stdy -> Obj(1,widget_info(widget_info(self.id,find_by_uname=List),/droplist_select))
	return, Region -> Name()
END

PRO PMI__Button__Display__Jeong_FitDualInletRoi::GET $
, 	CursorPos = CursorPos $
,	Model=Model, AIF_Delay=AIF_Delay, VIF_Delay=VIF_Delay $
, 	AIF_Use=AIF_Use, VIF_Use=VIF_Use $
,	Time=Time, Fit=Fit $
, 	RoiCurve=RoiCurve, AifCurve=AifCurve, VifCurve=VifCurve $
,	RoiName=RoiName, AifName=AifName, VifName=VifName $
, 	OnDisplay=OnDisplay

	if arg_present(CursorPos) then CursorPos=self.CursorPos

	if arg_present(Model) then begin
		list = widget_info(self.id,find_by_uname='FIT')
		widget_control, list, Get_Value=Models
		Model = Models[widget_info(list,/droplist_select)]
	endif

	if arg_present(AIF_Delay) then AIF_Delay = widget_info(widget_info(self.id,find_by_uname='AIF Delay'),/Button_set)
	if arg_present(VIF_Delay) then VIF_Delay = widget_info(widget_info(self.id,find_by_uname='VIF Delay'),/Button_set)
	if arg_present(AIF_Use) then AIF_Use = widget_info(widget_info(self.id,find_by_uname='AIF Use'),/Button_set)
	if arg_present(VIF_Use) then VIF_Use = widget_info(widget_info(self.id,find_by_uname='VIF Use'),/Button_set)

	if arg_present(Time) then begin
		t = Self.Series -> c(1)
		Time = t-t[0]
	endif

	if arg_present(RoiName) then RoiName=Self->GetName('ROI')
	if arg_present(AifName) then AifName=Self->GetName('AIF')
	if arg_present(VifName) then VifName=Self->GetName('VIF')

	if arg_present(RoiCurve) then $
		if ptr_valid(Self.Curve[0]) then RoiCurve = *Self.Curve[0] $
		else begin
			RoiCurve = Self->GetCurve('ROI')
			Self.Curve[0] = ptr_new(RoiCurve)
		endelse

	if arg_present(AifCurve) then $
		if ptr_valid(Self.Curve[1]) then AifCurve = *Self.Curve[1] $
		else begin
			AifCurve = Self->GetCurve('AIF')/(1-self.Hematocrit)
			Self.Curve[1] = ptr_new(AifCurve)
		endelse

	if arg_present(VifCurve) then $
		if ptr_valid(Self.Curve[2]) then VifCurve = *Self.Curve[2] $
		else begin
			VifCurve = Self->GetCurve('VIF')/(1-self.Hematocrit)
			Self.Curve[2] = ptr_new(VifCurve)
		endelse

	if arg_present(FIT) then begin
		if not ptr_valid(Self.Curve[3]) then Self->Fit
		FIT = *Self.Curve[3]
	endif

	if arg_present(OnDisplay) then begin
		OnDisplay = ''
		List = ['AIF','VIF','ROI','FIT']
		for i=0L,3 do $
		if 0 eq widget_info(widget_info(self.id,find_by_uname=List[i]+'bttn'),/sensitive) then OnDisplay=List[i]
	endif
END





PRO PMI__Button__Display__Jeong_FitDualInletRoi::SET, $
	PMI__REFRESH=pmi__refresh, PMI__RESIZE=pmi_resize, $
	Refresh=Refresh,	Erase=Erase, $
	Message=Message, Sensitive=Sensitive, $
 	xsize=xsize, ysize=ysize, $
	Set_droplist_select = Set_droplist_select, $
	Series=Series, Units=Units, Baseline=Baseline, Hematocrit=Hematocrit, $
	Relaxivity=Relaxivity, T1t=T1t, T1a=T1a, T1v=T1v, TR=TR, FA=FA, $
	OnDisplay=OnDisplay

	if keyword_set(pmi_resize) then begin
		gpmi = widget_info(/geometry, tlb(self.id))
		self -> Set, xsize=gpmi.xsize, ysize=gpmi.ysize, /REFRESH
	endif

	if n_elements(Series) 	ne 0 then Self.Series = Series
	if n_elements(Units) 	ne 0 then self.Units = Units
	if n_elements(Baseline) 	ne 0 then self.Baseline = Baseline
	if n_elements(Hematocrit) 	ne 0 then self.Hematocrit = Hematocrit
	if n_elements(Relaxivity) 	ne 0 then self.Relaxivity = Relaxivity
	if n_elements(T1t) 	ne 0 then self.T1t = T1t
	if n_elements(T1a) 	ne 0 then self.T1a = T1a
	if n_elements(T1v) 	ne 0 then self.T1v = T1v
	if n_elements(TR) 	ne 0 then self.TR = TR
	if n_elements(FA) 	ne 0 then self.FA = FA

	if n_elements(Message) 	ne 0 then begin
		Self -> SET, OnDisplay='', /Refresh
		xyouts, 0.1,0.9, Message, color=0,/normal,charsize=1.5,charthick=2.0
	endif
	if n_elements(Sensitive) ne 0 then widget_control, self.id, sensitive=sensitive
	if n_elements(Set_droplist_select) ne 0 then begin
		List = ['ROI','AIF','VIF']
		for i=0,2 do widget_control, $
			widget_info(self.id,find_by_uname=List[i]), $
			Set_droplist_select=Set_droplist_select[i]
	endif
	if n_elements(xsize) ne 0 $
	or n_elements(ysize) ne 0 then begin
		widget_control, self.DrawId, xsize=xsize, ysize=ysize-40
		xs = floor((xsize - 535)/4E)
		if xs lt 50 then xs = 50
		List = ['AIF','VIF','ROI','FIT']
		for i=0,3 do widget_control, widget_info(self.id,find_by_uname=List[i]), xsize=xs
	endif

	if keyword_set(Erase) then begin
		widget_control, self.DrawId, get_value = win
		wset, win & erase, 255
	endif

	if n_elements(OnDisplay) ne 0 then begin

		List = ['AIF','VIF','ROI','FIT']
		for i=0,3 do $
		widget_control, widget_info(self.id,find_by_uname=List[i]+'bttn'), Sensitive=List[i] ne OnDisplay
 		widget_control, widget_info(self.id,find_by_uname='Export')		, Sensitive=OnDisplay eq 'FIT'
		widget_control, widget_info(self.id,find_by_uname='Export As')	, Sensitive=OnDisplay eq 'FIT'
	endif

	if keyword_set(Refresh) then begin

		Menu = widget_info(widget_info(tlb(self.id),/child),/all_children)
		for i=0L,n_elements(Menu)-1 do widget_control, Menu[i], sensitive=0
		Self -> Plot
	endif

END





PRO PMI__Button__Display__Jeong_FitDualInletRoi::Cleanup
	widget_control, self.id, /destroy
	ptr_free, Self.Curve, self.Parameters
	loadct, 0
END

FUNCTION PMI__Button__Display__Jeong_FitDualInletRoi::Init, parent, CursorPos, xsize=xsize, ysize=ysize

	loadct, 12

	PMI__Info, tlb(parent), Stdy=Stdy

	if n_elements(CursorPos) ne 0 then self.CursorPos = CursorPos

	self.id = widget_base(parent,/column,map=0,event_func='PMI__Button__Display__Event__Jeong_FitDualInletRoi')

	Controls = widget_base(self.id,/row,ysize=40,/base_align_center,space=5)
	self.DrawId	= widget_draw(self.id,/retain)

		v = 'ROI'
		Base = widget_base(Controls,/row,/frame,/base_align_center)
		id = widget_button(Base, xsize=23, ysize=19, value=v, uname=v+'bttn')
  		id = widget_droplist(Base,/dynamic_resize, value=Stdy->Names(1), uname=v)

		v = ['AIF','VIF']
		delayset = [1,0]
		for i=0,1 do begin
			Base = widget_base(Controls,/row,/frame,/base_align_center)
			TickBase = widget_base(Base,/row,/nonexclusive, uname=v[i]+'baseUse')
			id = widget_button(TickBase, value='', uname=v[i]+' Use')
			widget_control, id, /set_button, sensitive=0
			id = widget_button(Base, xsize=23, ysize=19, value=v[i], uname=v[i]+'bttn')
  			id = widget_droplist(Base,/dynamic_resize, value=Stdy->Names(1), uname=v[i])
  			TickBase = widget_base(Base,/row,/nonexclusive, uname=v[i]+'baseDel')
  			id = widget_button(TickBase, value='Del', uname=v[i]+' Delay')
			widget_control, id, set_button=delayset[i], sensitive=0
  		endfor

		Base = widget_base(Controls,/row,/frame,/base_align_center)
			id = widget_button(Base, xsize=25, ysize=19, value='FIT', uname='FITbttn')
  			id = widget_droplist(Base,/dynamic_resize, uname='FIT',value = ['Compartment','Patlak','Modified Tofts', 'Uptake','Filtration','Exchange','Exchange-Uptake'])
;  			id = widget_droplist(Base,/dynamic_resize, uname='FIT',value = ['Compartment'])
   			widget_control, id, set_droplist_select=0, sensitive=0

		v = ['Export','Export As','Close']
		Base = widget_base(Controls,/row,/frame,/base_align_center)
		for i=0,2 do id = widget_button(Base, xsize=50, ysize=22, value=v[i], uname=v[i])

	self -> Set, xsize=xsize, ysize=ysize, OnDisplay='FIT'

	widget_control, self.id, set_uvalue = self, /map
	return, 1
END

PRO PMI__Button__Display__Jeong_FitDualInletRoi__Define

	DualInletCompartment
	DualInletPatlak
	DualInletModTofts
	DualInletUptake
	DualInletFiltration
	DualInletExchange
	DualInletExchangeUptake

	Struct = {PMI__Button__Display__Jeong_FitDualInletRoi 	$
	,	id: 0L 	$
	,	DrawId: 0L $
	,	CursorPos:lonarr(4)	$
	,	Curve:ptrarr(4) $ ;RoiCurve, AifCurve, VifCurve, Fit
	,	Parameters: ptr_new() $
	,	Units: '' $
	,	Series: obj_new() $
	,	Baseline: 0L $
	,	Hematocrit: 0E $
	,	Relaxivity:0E $
	, 	T1t:0E $
	, 	T1a:0E $
	, 	T1v:0E $
	,	TR:0E $
	, 	FA:0E $
	}
END


pro PMI__Button__Event__Jeong_FitDualInletRoi, ev

	PMI__Info, ev.top, Stdy=Stdy

    Series = Stdy->Names(0,ns,DefDim=3,ind=ind,sel=sel)
    Regions = Stdy->Names(1,nr)

	v = PMI__Form(ev.top, Title='Perfusion analysis setup', [$
		ptr_new({Type:'DROPLIST',Tag:'series', Label:'Dynamic series', Value:Series, Select:sel}), $
		ptr_new({Type:'DROPLIST',Tag:'aif'	 , Label:'Arterial Region', Value:Regions, Select:stdy->sel(1)}), $
		ptr_new({Type:'DROPLIST',Tag:'vif'	 , Label:'Venous Region', Value:Regions, Select:stdy->sel(1)}), $
		ptr_new({Type:'DROPLIST',Tag:'roi'	 , Label:'Tissue Region', Value:Regions, Select:stdy->sel(1)}), $
		ptr_new({Type:'VALUE'	,Tag:'nt' 	 , Label:'Length of baseline (sec)', Value:10.0}),$
		ptr_new({Type:'VALUE'	,Tag:'hct'	 , Label:'Patients hematocrit', Value:0.45})])
	IF v.cancel THEN return

    Series = Stdy->Obj(0,ind[v.series])
    time = Series->t() - Series->t(0)

	T1a = 1000.0 / (0.52 * v.hct + 0.38)  ; Lu MRM 2004
	T1v = 1000.0 / (0.83 * v.hct + 0.28)  ; Lu MRM 2004
	T1t = 809E ;sec
	Relaxivity = 3.6E ;Hz/mM

	PMI__Control, ev.top, Viewer = 'PMI__Button__Display__Jeong_FitDualInletRoi', Display=Display

	Display -> Set, $
		Series = Series, $
		Baseline = ceil(v.nt/time[1]), $
		Units = 'Linear (mM)', $
		Hematocrit = v.hct, $
		set_droplist_select = [v.roi,v.aif,v.vif]

	Display -> Set, Relaxivity=Relaxivity, T1t=T1t, T1a=T1a, T1v=T1v
	Display -> Set, /Refresh
end

pro PMI__Button__Control__Jeong_FitDualInletRoi, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 2)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__Jeong_FitDualInletRoi, parent,value=value, separator=separator

	PMI__Button__Display__Jeong_FitDualInletRoi__Define

	if n_elements(value) eq 0 then value = 'Fit dual-inlet models (ROI)'

	id = widget_button(parent $
	,	value = value	$
	,	event_pro = 'PMI__Button__Event__Jeong_FitDualInletRoi'	$
	,	pro_set_value = 'PMI__Button__Control__Jeong_FitDualInletRoi' $
	, 	separator = separator)

	return, id
end
