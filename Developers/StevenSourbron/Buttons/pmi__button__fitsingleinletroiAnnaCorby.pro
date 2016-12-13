PRO PMI__Button__FitSingleInletRoiAnnaCorby__Display::Fit

	Self->GET, Model=Model, Positivity=Pos, Delay=Delay, Time=Time, AifCurve=Aif, RoiCurve=Curve, OnDisplay=OnDisplay
	Self->SET, Message='Fitting...', Sensitive=0

	IF Delay NE 0 THEN BEGIN
		If keyword_set(Pos) then Delay0=0 Else Delay0=-20
		DELAY_VALUES=[Delay0,20,time[1]/2E]
	ENDIF

	CASE Model OF

		'Maximum slope':begin
			Fit = MaximumSlopePerfusion(time, curve, AIF, flow = flow)
			aic = n_elements(time)*alog(total((curve-Fit)^2)/n_elements(time)) + 2D*(1+3)
			Pd=0E
			Parameters = $
				[{Name:'Blood Flow'		,Units:'ml/100ml/min'	,Value:6000D*flow/(1D -self.Hematocrit),Nr: 0}	]
			end

		'1C Steady State':begin
			P = [0.2] ;[V]
			Fit = FitSingleInlet('SteadyState', time, aif, curve, P, DELAY_PAR=Pd, DELAY_VALUES=DELAY_VALUES, AKAIKE_ERROR=aic, POSITIVITY=Pos, /NODERIVATIVE, LIMITED_ABOVE=[1])
			Parameters = $
				[{Name:'Extracellular Volume'		,Units:'ml/100ml'		,Value:100D*P[0]	,Nr: 4} ]
			end

		'Patlak':begin
			P = [0.1, 12.0/6000] ;[VP, FE]
			Fit = FitSingleInlet('Patlak', time, aif, curve, P, DELAY_PAR=Pd, DELAY_VALUES=DELAY_VALUES, AKAIKE_ERROR=aic, POSITIVITY=Pos, /NODERIVATIVE, LIMITED_ABOVE=[1,0])
			Parameters = $
				[{Name:'Blood Volume'		,Units:'ml/100ml'		,Value:100D*P[0]/(1-self.Hematocrit)	,Nr: 1} $
				,{Name:'Ktrans'	,Units:'ml/100ml/min'	,Value:6000D*P[1]	,Nr: 7} ]
			end

		'Model-free':begin
			IRF = DeconvolveCurve(time,	curve, aif, dt=dt, Fit=Fit, pc='GCV', wm=1L, m0=0.001, m1=1.0, nm=100L, Quad='O2')
			aic = n_elements(time)*alog(total((curve-Fit)^2)/n_elements(time)) + 2D*(1+2)
			Pd=0E
			Parameters = $
				[{Name:'Blood Flow'			,Units:'ml/100ml/min'	,Value:6000D*max(IRF)/(1-self.Hematocrit)			,Nr: 0} $
				,{Name:'Extracellular MTT'			,Units:'sec'			,Value:1D*dt*total(IRF)/max(IRF)		,Nr: 3} $
				,{Name:'Extracellular Volume'		,Units:'ml/100ml'		,Value:100D*dt*total(IRF)	,Nr: 4} ]
			end

		'Compartment':begin
			P = [0.3, 120.0/6000] ;[V, F]
			Fit = FitSingleInlet('Compartment', time, aif, curve, P, DELAY_PAR=Pd, DELAY_VALUES=DELAY_VALUES, AKAIKE_ERROR=aic, POSITIVITY=Pos, /NODERIVATIVE, LIMITED_ABOVE=[1,0])
			Parameters = $
				[{Name:'Blood Flow'				,Units:'ml/100ml/min'	,Value:6000D*P[1]/(1-self.Hematocrit)	,Nr: 0} $
				,{Name:'Extracellular MTT'			,Units:'sec'			,Value:1D*P[0]/P[1] ,Nr: 3} $
				,{Name:'Extracellular Volume'		,Units:'ml/100ml'		,Value:100D*P[0]	,Nr: 4} ]
			end

		'Modified Tofts':begin
			P = [0.3, 2.0/3, 12.0/6000] 	;[VP+VE, VE/(VP+VE), FE]
			Fit = FitSingleInlet('ModifiedTofts', time, aif, curve, P, DELAY_PAR=Pd, DELAY_VALUES=DELAY_VALUES, AKAIKE_ERROR=aic, POSITIVITY=Pos, /NODERIVATIVE, LIMITED_ABOVE=[1,1,0])

;		vp = P[0]*(1-P[1]) & ve=P[0]*P[1] & Ktrans=P[2] & print, 100*vp, 100*ve, 6000*Ktrans ;PMI
;		vp = 0.55*23.84/100/0.73 & ve=14.3/100 & Ktrans=2.1/6000 ;ICE
;		vp = 0.55*8.59/100 & ve=15.1/100 & Ktrans=2.1/6000 ;OLEA
;		Fit = vp*aif + Ktrans*ExpConvolution(Ktrans/ve,[time,aif])
;		Fit = (time[1]-time[0])*convolution_matrix(aif,QUAD='O1') ## exp(-time*Ktrans/ve)

			Parameters = $
				[{Name:'Blood Volume'			,Units:'ml/100ml'		,Value:100D*P[0]*(1-P[1])/(1-self.Hematocrit)	,Nr: 0} $
				,{Name:'Interstitial MTT'		,Units:'sec'			,Value:1D*P[0]*P[1]/P[2]		,Nr: 3} $
				,{Name:'Interstitial Volume'	,Units:'ml/100ml'		,Value:100D*P[0]*P[1]			,Nr: 4} $
				,{Name:'Ktrans'					,Units:'ml/100ml/min'	,Value:6000D*P[2]	,Nr: 7} ]
			end

		'2C Uptake':begin
			P = [0.1, 120.0/6000, 12/132.] ;[VP, FP, FE/(FP+FE)]
			Fit = FitSingleInlet('2CUptakeExchange', time, aif, curve, P, DELAY_PAR=Pd, DELAY_VALUES=DELAY_VALUES, AKAIKE_ERROR=aic, POSITIVITY=Pos, /NODERIVATIVE, LIMITED_ABOVE=[1,0,1])
			Parameters = $
				[{Name:'Blood Flow'			,Units:'ml/100ml/min'	,Value:6000D*P[1]/(1-self.Hematocrit)		,Nr: 0} $
				,{Name:'Blood MTT'				,Units:'sec'			,Value:1D*P[0]/P[1] 		,Nr: 1} $
				,{Name:'Blood Volume'			,Units:'ml/100ml'		,Value:100D*P[0]/(1-self.Hematocrit)		,Nr: 2} $
				,{Name:'Extraction Fraction'	,Units:'%' 				,Value:100D*P[2]	,Nr: 5} $
				,{Name:'Permeability-surface area product'		,Units:'ml/100ml/min'	,Value:6000D*P[1]*P[2]/(1-P[2])		,Nr: 6} $
				,{Name:'Ktrans'		,Units:'ml/100ml/min'	,Value:6000D*P[2]*P[1]			,Nr: 7}]
			end

		'2C Exchange':begin
 			P = [0.3, 0.02, 2.0/3, 0.1] ;[VP+VE, FP, VE/(VP+VE), FE/(FP+FE)]
			Fit = FitSingleInlet('Exchange', time, aif, curve, P, DELAY_PAR=Pd, DELAY_VALUES=DELAY_VALUES, AKAIKE_ERROR=aic, POSITIVITY=Pos, /NODERIVATIVE, LIMITED_ABOVE=[1,0,1,1])
			Parameters = $
				[{Name:'Blood Flow'				,Units:'ml/100ml/min'	,Value:6000D*P[1]/(1-self.Hematocrit)			,Nr: 0} $
				,{Name:'Blood MTT'				,Units:'sec'			,Value:1D*P[0]*(1-P[2])/P[1] 			,Nr: 1} $
				,{Name:'Blood Volume'			,Units:'ml/100ml'		,Value:100D*P[0]*(1-P[2])/(1-self.Hematocrit)			,Nr: 2} $
				,{Name:'Interstitial MTT'		,Units:'sec'			,Value:1D*(1-P[3])*P[0]*P[2]/(P[1]*P[3])			,Nr: 3} $
				,{Name:'Interstitial Volume'	,Units:'ml/100ml'		,Value:100D*P[0]*P[2]			,Nr: 4} $
				,{Name:'Extraction Fraction'	,Units:'%' 				,Value:100D*P[3]				,Nr: 5} $
				,{Name:'Permeability-surface area product'		,Units:'ml/100ml/min'	,Value:6000D*P[1]*P[3]/(1-P[3])			,Nr: 6} $
				,{Name:'Ktrans'		,Units:'ml/100ml/min'	,Value:6000D*P[3]*P[1]			,Nr: 7} ]
			end
	endcase

	Parameters = [Parameters,{Name:'Akaike Fit Error', Units:'', Value:AIC, Nr:14} ]
	IF Delay NE 0 THEN $
	Parameters = [Parameters,{Name:'Arterial Delay', Units:'sec', Value:1D*Pd, Nr:8} ]

	self.Curve[2] = ptr_new(Fit)
	self.Parameters = ptr_new(Parameters)
	Self->SET, OnDisplay=OnDisplay, /Sensitive
END

PRO PMI__Button__FitSingleInletRoiAnnaCorby__Display::Plot

	Self->GET, OnDisplay=OnDisplay

	top=0.9 & dy=0.04 & x0=0.525 & charsize=1.0 & charthick=1.0

	CASE OnDisplay OF

		'':Self->Set, /Erase

		'ROI':begin
			Self -> GET, Time=Time, RoiCurve=Y, RoiName=RoiName, ytitle=Ytitle
			Self -> SET, /Erase
 			plot, /nodata, position=[0.1,0.2,0.5,0.9]  $
			, 	[0,max(time)], [min(Y),max(Y)] $
			, 	/xstyle, /ystyle $
			, 	background=255, color=0 $
			, 	xtitle = 'Time (sec)', ytitle=Ytitle $
			, 	charsize=1.5, charthick=2.0, xthick=2.0, ythick=2.0
			oplot, time, Y, color=6*16, linestyle=0, thick=2
			xyouts, x0, top-0*dy, 'Region Of Interest: ' + RoiName, color=6*16, /normal, charsize=1.5, charthick=1.5
			end

		'AIF':begin
			Self -> GET, Time=Time, AifCurve=Y, AifName=AifName, ytitle=Ytitle
			Self -> SET, /Erase
 			plot, time, Y, position=[0.1,0.2,0.5,0.9]  $
			, 	/xstyle, /ystyle $
			, 	background=255, color=0 $
			, 	xtitle = 'Time (sec)', ytitle=Ytitle $
			, 	linestyle=0, thick=2 $
			, 	charsize=1.5, charthick=2.0, xthick=2.0, ythick=2.0
			xyouts, x0, top-1*dy, 'Arterial Input Function: ' + AifName	, color=0, /normal, charsize=1.5, charthick=1.5
			end

		'FIT':BEGIN
			Self -> GET, RoiCurve=Curve, Time=Time, Fit=Fit, Model=Model, RoiName=RoiName, AifName=AifName, Ytitle=Ytitle
			Self -> SET, /Erase

 			plot, /nodata, position=[0.1,0.2,0.5,0.9]  $
			, 	[0,max(time)], [min([min(Curve),min(Fit)]),max([max(Curve),max(Fit)])] $
			, 	/xstyle, /ystyle $
			, 	background=255, color=0 $
			, 	xtitle = 'Time (sec)', ytitle=Ytitle $
			, 	charsize=1.5, charthick=2.0, xthick=2.0, ythick=2.0
			oplot, time, Curve, color=6*16, psym=4, thick=2
			oplot, time, Fit, color=12*16, linestyle=0, thick=2

			xyouts, x0, top-0*dy, 'Region Of Interest: ' + RoiName		, color=6*16, /normal, charsize=1.5, charthick=1.5
			xyouts, x0, top-1*dy, 'Arterial Input Function: ' + AifName	, color=0, /normal, charsize=1.5, charthick=1.5
			xyouts, x0, top-2*dy, 'Tissue Model: ' + Model				, color=12*16, /normal, charsize=1.5, charthick=1.5

			P = *self.Parameters

			for i=0L,n_elements(P)-1 do xyouts $
				, x0, top-dy*(5+P[i].Nr) $
				, P[i].Name + ' = ' + PMI__Round(P[i].Value,3,/string) + ' ' + P[i].Units $
				, color=0, /normal, charsize=charsize, charthick=charthick
			END
	ENDCASE

END




FUNCTION PMI__Button__FitSingleInletRoiAnnaCorby__Display::Event, ev

	Uname = widget_info(ev.id,/uname)

	i = where(Uname Eq ['ROI','AIF','FIT']+'bttn', cnt)
	If cnt eq 1 then begin
		Self->SET, OnDisplay=strmid(Uname,0,3), /Refresh
		return, 0B
	endif

	i = where(Uname Eq ['ROI','AIF'], cnt)
	If cnt eq 1 then begin
		ptr_free, Self.Curve[i], Self.Curve[2], self.parameters
		Self->SET, OnDisplay=Uname, /Refresh
		return, 0B
	endif

	i = where(Uname Eq ['SIG','FIT','Delay','Positive'], cnt)
	if cnt eq 1 then begin
		ptr_free, Self.Curve[2], self.parameters
		Self->SET, OnDisplay='ROI', /Refresh
		return, 0B
	endif

	i = where(Uname Eq ['Export','Export As'], cnt)
	if cnt eq 1 then begin
		Self->GET, Time=Time, RoiCurve=RoiCurve, Fit=Fit, Model=Model, Roiname=Roiname
		if Uname eq 'Export' then begin
			PMI__Info, ev.top, Stdy=Stdy
			Path = Stdy->Datapath() + 'Single-Inlet Models (ROI)'
			file_mkdir, Path
			File = Path + '\' + Roiname + '__SI_' + Model
		endif else begin
			PMI__Info, ev.top, State=State
			if not State -> Get_file(file, file=cleanstr(Roiname + '__' + Model), title='Save as..', filter='.tif') then return,0B
			File = strmid(file,0,strlen(file)-4)
		endelse
		Write_tiff, File + '.tif', reverse(tvrd(/true),3)
		nT = n_elements(Time)
		nP = n_elements(*self.Parameters)
		PlotData = strarr(5,1+max([nT,nP]))
		PlotData[*,0]=['Time','ROI curve','ROI fit','Parameters','Fitted Values']
		PlotData[0,1:nT]=strcompress(Time,/remove_all)
		PlotData[1,1:nT]=strcompress(RoiCurve,/remove_all)
		PlotData[2,1:nT]=strcompress(Fit,/remove_all)
		PlotData[3,1:nP]= (*self.Parameters).Name + '  (' + (*self.Parameters).Units + ')'
		PlotData[4,1:nP]= strcompress((*self.Parameters).Value,/remove_all)
		PMI__Write_csv, File + '__Fit.csv', PlotData
		return, 0B
	endif

	Menu = widget_info(widget_info(ev.top,/child),/all_children)
	for i=0L,n_elements(Menu)-1 do widget_control, Menu[i], /sensitive
	PMI__Control, ev.top, Viewer = 'PMI__DISPLAY__2DVIEW'
	PMI__Control, ev.top, /refresh
	return, 0B
END

FUNCTION PMI__Button__FitSingleInletRoiAnnaCorby__Display__Event, ev

	widget_control, ev.handler, get_uvalue=self
	return, Self -> Event(ev)
END


FUNCTION PMI__Button__FitSingleInletRoiAnnaCorby__Display::Conc, Signal, List

	S0 = total(Signal[0:self.baseline-1])/self.baseline
	Self -> GET, ConcModel=ConcModel
	If ConcModel eq 0 then return, Signal-S0
	If ConcModel eq 1 then return, 100*(Signal-S0)/S0
	If ConcModel eq 4 then return, remove_inf(-alog(Signal/S0))
	if List Eq 'ROI' then T1 = 1.0 else T1=1.441
	relaxivity = 3.6
	if ConcModel eq 2 then return, (1/(relaxivity*T1))*(Signal-S0)/S0
	TR = self.series->GETVALUE('0018'x,'0080'x)
	FA = self.series->GETVALUE('0018'x,'1314'x)
	Conc = Concentration_SPGRESS(Signal, S0, T1*1000, FA, TR, relaxivity)
	return, Conc
END

FUNCTION PMI__Button__FitSingleInletRoiAnnaCorby__Display::GetRegion, List

	PMI__Info, tlb(self.id), Stdy=Stdy
	select = widget_info(widget_info(self.id,find_by_uname=List),/droplist_select)
	if select GE 0 then return, Stdy->Obj(1,select)
	return, 0
END

FUNCTION PMI__Button__FitSingleInletRoiAnnaCorby__Display::GetCurve, List

	Region = Self->GetRegion(List)
	if not obj_valid(Region) then return, 0
	Self -> GET, OnDisplay=OnDisplay, Time=Time, Conc=Conc
	Self -> SET, Message = 'Loading ' + Region->Name() + ' curve', Sensitive=0
	PMI__Info, tlb(self.id), Stdy=Stdy
	Curve = PMI__RoiCurve(Stdy->DataPath(), Self.Series, Region, cnt=cnt)
	if cnt eq 0 then Curve = PMI__RoiCurveIndices(Stdy->DataPath(), Self.Series, Region, cnt=cnt)
	Self -> SET, OnDisplay=OnDisplay, /Sensitive
	if cnt eq 0 then return, Time*0
	return, Curve
END


FUNCTION PMI__Button__FitSingleInletRoiAnnaCorby__Display::GetName, List

	PMI__Info, tlb(self.id), Stdy=Stdy
	Region = Stdy -> Obj(1,widget_info(widget_info(self.id,find_by_uname=List),/droplist_select))
	return, Region -> Name()
END


PRO PMI__Button__FitSingleInletRoiAnnaCorby__Display::GET $
, 	CursorPos = CursorPos $
,	Model=Model, Positivity=Positivity, Delay=Delay $
,	Time=Time, Fit=Fit $
, 	RoiCurve=RoiCurve, AifCurve=AifCurve $
,	Roiname=Roiname, AifName=AifName $
, 	OnDisplay=OnDisplay, ConcModel=ConcModel, ytitle=Ytitle

	if arg_present(CursorPos) then CursorPos=self.CursorPos

	if arg_present(Model) then begin
		list = widget_info(self.id,find_by_uname='FIT')
		widget_control, list, Get_Value=Models
		Model = Models[widget_info(list,/droplist_select)]
	endif
	if arg_present(ConcModel) then $
		ConcModel = widget_info(widget_info(self.id,find_by_uname='SIG'),/droplist_select)
	if arg_present(Ytitle) then begin
		self->Get, ConcModel=C
		Case C of
		0:Ytitle = 'Concentration (a.u.)'
		1:Ytitle = 'Concentration (%)'
		2:Ytitle = 'Concentration (mM)'
		3:Ytitle = 'Concentration (mM)'
		4:Ytitle = 'Concentration (dimensionless)'
		Endcase
	endif

	if arg_present(Positivity) then $
		Positivity = widget_info(widget_info(self.id,find_by_uname='Positive'),/Button_set)
	if arg_present(Delay) then $
		Delay = widget_info(widget_info(self.id,find_by_uname='Delay'),/Button_set)
	if arg_present(Time) then begin
		t = Self.Series -> c(1)
		Time = t-t[0]
	endif

	if arg_present(RoiName) then RoiName=Self->GetName('ROI')
	if arg_present(AifName) then AifName=Self->GetName('AIF')

	if arg_present(RoiCurve) then begin
		if ptr_valid(Self.Curve[0]) then begin
			RoiCurve = *Self.Curve[0]
		endif else begin
			RoiCurve = Self->GetCurve('ROI')
			Self.Curve[0] = ptr_new(RoiCurve)
		endelse
		RoiCurve = Self->Conc(RoiCurve,'ROI')
	endif

	if arg_present(AifCurve) then begin
		if ptr_valid(Self.Curve[1]) then begin
			AifCurve = *Self.Curve[1]
		endif else begin
			AifCurve = Self->GetCurve('AIF')
			Self.Curve[1] = ptr_new(AifCurve)
		endelse
		AifCurve = Self->Conc(AifCurve,'AIF')/(1-self.Hematocrit)
	endif

	if arg_present(FIT) then begin
		if not ptr_valid(Self.Curve[2]) then Self->Fit
		FIT = *Self.Curve[2]
		endif

	if arg_present(OnDisplay) then begin
		OnDisplay = ''
		List = ['AIF','ROI','FIT']
		for i=0L,2 do $
		if 0 eq widget_info(widget_info(self.id,find_by_uname=List[i]+'bttn'),/sensitive) then OnDisplay=List[i]
	endif

END



PRO PMI__Button__FitSingleInletRoiAnnaCorby__Display::SET, $
	PMI__REFRESH=pmi__refresh, PMI__RESIZE=pmi_resize, $
	Refresh=Refresh, Erase=Erase, $
	Message=Message, Sensitive=Sensitive, $
	xsize=xsize, ysize=ysize, $
	Set_droplist_select = Set_droplist_select, $
	Series=Series, Baseline=Baseline, Hematocrit=Hematocrit, $
	OnDisplay=OnDisplay

	if keyword_set(pmi_resize) then begin
		gpmi = widget_info(/geometry, tlb(self.id))
		self -> Set, xsize=gpmi.xsize, ysize=gpmi.ysize, /REFRESH
	endif

	if n_elements(Series) 	ne 0 then Self.Series = Series
	if n_elements(Baseline) 	ne 0 then self.Baseline = Baseline
	if n_elements(Hematocrit) 	ne 0 then self.Hematocrit = Hematocrit
	if n_elements(Message) 	ne 0 then begin
		Self -> SET, OnDisplay='', /Refresh
		xyouts, 0.1,0.9, Message, color=0,/normal,charsize=1.5,charthick=2.0
	endif
	if n_elements(Sensitive) ne 0 then widget_control, self.id, sensitive=sensitive
	if n_elements(Set_droplist_select) ne 0 then begin
		List = ['ROI','AIF']
		for i=0,1 do widget_control, $
			widget_info(self.id,find_by_uname=List[i]), $
			Set_droplist_select=Set_droplist_select[i]
	endif
	if n_elements(xsize) ne 0 $
	or n_elements(ysize) ne 0 then begin
		widget_control, self.DrawId, xsize=xsize, ysize=ysize-40
		xs = floor((xsize - 750)/2E)
		if xs lt 50 then xs = 50
		List = ['ROI','AIF']
		for i=0,1 do widget_control, widget_info(self.id,find_by_uname=List[i]), xsize=xs
	endif

	if keyword_set(Erase) then begin
		widget_control, self.DrawId, get_value = win
		wset, win & erase, 255
	endif

	if n_elements(OnDisplay) ne 0 then begin

		List = ['AIF','ROI','FIT']
		for i=0,2 do $
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





PRO PMI__Button__FitSingleInletRoiAnnaCorby__Display::Cleanup
	widget_control, self.id, /destroy
	ptr_free, Self.Curve, self.Parameters
	loadct, 0
END

FUNCTION PMI__Button__FitSingleInletRoiAnnaCorby__Display::Init, parent, CursorPos, xsize=xsize, ysize=ysize

	if n_elements(CursorPos) ne 0 then self.CursorPos = CursorPos

	loadct, 12

	PMI__Info, tlb(parent), Stdy=Stdy

	self.id = widget_base(parent,/column,map=0,event_func='PMI__Button__FitSingleInletRoiAnnaCorby__Display__Event')
	Controls = widget_base(self.id,/row,ysize=40,/base_align_center,space=5)
	self.DrawId	= widget_draw(self.id,/retain)

		v = ['ROI','AIF']
		for i=0,1 do begin
			Base = widget_base(Controls,/row,/frame,/base_align_center)
			id = widget_button(Base, xsize=25, ysize=19, value=v[i], uname=v[i]+'bttn')
  			id = widget_droplist(Base,/dynamic_resize, value=Stdy->Names(1), uname=v[i])
  		endfor

		Base = widget_base(Controls,/row,/frame,/base_align_center)
			id = widget_droplist(Base,/dynamic_resize, uname='SIG',value = ['Linear (a.u.)','Linear (%)','Linear (mM)', 'Non-linear (mM)', 'DSC-MRI'])
  			widget_control, id, set_droplist_select = 0

		Base = widget_base(Controls,/row,/frame,/base_align_center)
			id = widget_button(Base, xsize=25, ysize=19, value='FIT', uname='FITbttn')
  			id = widget_droplist(Base,/dynamic_resize, uname='FIT',value = ['Maximum slope','Model-free', '1C Steady State','Compartment', 'Patlak','Modified Tofts','2C Uptake','2C Exchange'])
  			widget_control, id, set_droplist_select = 5

		v = ['Positive','Delay']
		for i=0,1 do begin
			Base = widget_base(Controls,/row,/frame,/nonexclusive)
			id = widget_button(Base, value=v[i], uname=v[i])
		endfor

		v = ['Export','Export As','Close']
		Base = widget_base(Controls,/row,/frame,/base_align_center)
		for i=0,2 do id = widget_button(Base, xsize=50, ysize=22, value=v[i], uname=v[i])

	self -> Set, xsize=xsize, ysize=ysize, OnDisplay='FIT'
	widget_control, self.id, set_uvalue = self, /map
	return, 1
END

PRO PMI__Button__FitSingleInletRoiAnnaCorby__Display__Define

	Struct = {PMI__Button__FitSingleInletRoiAnnaCorby__Display 	$
	,	id: 0L 	$
	,	DrawId: 0L $
	,	CursorPos:lonarr(4)	$
	,	Curve:ptrarr(3) $ ;RoiCurve, AifCurve, Fit
	,	Parameters: ptr_new() $
	,	Series: obj_new() $
	,	Baseline: 0L $
	,	Hematocrit: 0E $
	}
END



FUNCTION PMI__Button__Input__FitSingleInletRoiAnnaCorby, top, series, in

    PMI__Info, top, Stdy=Stdy
    DynSeries = Stdy->Names(0,DefDim=3,ind=ind,sel=sel)
	in = {ser:sel, aif:stdy->sel(1), roi:stdy->sel(1), nb:5, hct:0.45}

	WHILE 1 DO BEGIN

		in = PMI__Form(top, Title='Perfusion analysis setup', [$
		ptr_new({Type:'DROPLIST',Tag:'ser', Label:'Dynamic series', Value:DynSeries, Select:in.ser}), $
		ptr_new({Type:'DROPLIST',Tag:'aif', Label:'Arterial Region', Value:Stdy->names(1), Select:in.aif}), $
		ptr_new({Type:'DROPLIST',Tag:'roi', Label:'Tissue Region', Value:[Stdy->names(1)], Select:in.roi}), $
		ptr_new({Type:'VALUE'	,Tag:'nb' , Label:'Length of baseline (# of dynamics)', Value:in.nb}),$
		ptr_new({Type:'VALUE'	,Tag:'hct', Label:'Patients hematocrit', Value:in.hct})])
		IF in.cancel THEN return, 0

    	Series = Stdy->Obj(0,ind[in.ser])
    	IF (in.nb LT 1) or (in.nb GT Series->d(3)) THEN BEGIN
    		in.nb = 10
    		msg = ['Baseline length must be less than the total number of dynamics','Please select another baseline length']
    		IF 'Cancel' EQ dialog_message(msg,/information,/cancel) THEN BREAK ELSE CONTINUE
    	ENDIF

    	return, 1
  	ENDWHILE
  	return, 0
END

pro PMI__Button__Event__FitSingleInletRoiAnnaCorby, ev

	IF NOT PMI__Button__Input__FitSingleInletRoiAnnaCorby(ev.top, series, in) THEN return

	PMI__Control, ev.top, Viewer = 'PMI__Button__FitSingleInletRoiAnnaCorby__Display', Display=Display

	Display -> Set, /Refresh, $
		Series = Series, $
		Baseline = in.nb, $
		Hematocrit = in.hct, $
		set_droplist_select = [in.roi,in.aif]
end

pro PMI__Button__Control__FitSingleInletRoiAnnaCorby, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 1)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__FitSingleInletRoiAnnaCorby, parent,value=value, separator=separator

	SingleInletSteadyState
	SingleInletPatlak
	SingleInletCompartment
	SingleInletModifiedTofts
	SingleInlet2CUptakeExchange
	SingleInletExchange

	if n_elements(value) eq 0 then value = 'Fit single-inlet exchange models (ROI)'

	id = widget_button(parent 	$
	,	value = value	$
	,	event_pro = 'PMI__Button__Event__FitSingleInletRoiAnnaCorby'	$
	,	pro_set_value = 'PMI__Button__Control__FitSingleInletRoiAnnaCorby' $
	, 	separator = separator )
	return, id
end
