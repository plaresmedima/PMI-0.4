PRO PMI__Display__CardiacSingleInletRoiSliderConc::Fit

	Self->GET, Model=Model, Delay=Delay, Time=Time, AifCurve=Aif, RoiCurve=Curve, Indices=ind, OnDisplay=OnDisplay
	Self->SET, Sensitive=0

	IF Delay NE 0 THEN Steps = [0,3,0.25]

	ti = ind[0]+lindgen(1+ind[1]-ind[0])

	IF Model EQ 'Uptake' THEN BEGIN

		P = [0.015] ;[FP]
		Fit = FitSingleInlet('Uptake',time,aif,curve,P, INDICES=ti, $
			DELAY_PAR=Pd, DELAY_VALUES=steps, AKAIKE_ERROR=aic, /POSITIVITY, LIMITED_ABOVE=[0], /noderivative)
		Parameters = $
			[{Name:'Blood Flow'		,Units:'ml/min/g'	,Value:60D*P[0]/0.55	,Nr: 0}]
	ENDIF

	IF Model EQ 'Compartment' THEN BEGIN

		P = [0.3, 0.015] ;[V, F]
		Fit = FitSingleInlet('Compartment',time,aif,curve,P, INDICES=ti, $
			DELAY_PAR=Pd, DELAY_VALUES=steps, AKAIKE_ERROR=aic, /POSITIVITY, LIMITED_ABOVE=[1,0], /noderivative)
		Parameters = $
			[{Name:'Blood Flow'		,Units:'ml/min/g'	,Value:60D*P[1]/0.55	,Nr: 0} $
			,{Name:'Extracellular Volume'	,Units:'ml/100ml'	,Value:100D*P[0]		,Nr: 1} $
			,{Name:'Extracellular MTT'	,Units:'sec'				,Value:1D*P[0]/P[1]			,Nr: 2} ]
	ENDIF

	IF Model EQ 'Two-compartment' THEN BEGIN

		P = [0.3, 0.015, 2.0/3, 0.1] ;[VP+VE, FP, VE/(VP+VE), FE/(FP+FE)]
		Fit = FitSingleInlet('Exchange',time,aif,curve,P, INDICES=ti, $
			DELAY_PAR=Pd, DELAY_VALUES=steps, AKAIKE_ERROR=aic, /POSITIVITY, LIMITED_ABOVE=[1,0,1,1], /noderivative)
		Parameters = $
			[{Name:'Blood Flow'		,Units:'ml/min/g'	,Value:60D*P[1]/0.55	,Nr: 0} $
			,{Name:'Blood MTT'		,Units:'sec'			,Value:1D*P[0]*(1-P[2])*(1-P[3])/P[1] 			,Nr: 1} $
			,{Name:'Blood Volume'	,Units:'ml/100ml'		,Value:100D*P[0]*(1-P[2])/0.55			,Nr: 2}  $
			,{Name:'Interstitial MTT'		,Units:'sec'			,Value:1D*(1-P[3])*P[0]*P[2]/(P[1]*P[3])			,Nr: 3}$
			,{Name:'Interstitial Volume'	,Units:'ml/100ml'		,Value:100D*P[0]*P[2]			,Nr: 4}$
			,{Name:'Extraction Fraction'	,Units:'%' 				,Value:100D*P[3]				,Nr: 5} $
			,{Name:'Permeability-surface area product'		,Units:'ml/100ml/min'	,Value:6000D*P[1]*P[3]/(1-P[3])			,Nr: 6}$
			,{Name:'Ktrans'		,Units:'ml/100ml/min'	,Value:6000D*P[3]*P[1]			,Nr: 7}]
	ENDIF

	IF Model EQ 'Fermi' THEN BEGIN

		P = [0.015, 1.0, 0.5] ;[FP, a, b]
		Fit = FitSingleInlet('Fermi',time,aif,curve,P, INDICES=ti, $
			DELAY_PAR=Pd, DELAY_VALUES=steps, AKAIKE_ERROR=aic, /noderivative)
		Parameters = $
			[{Name:'Blood Flow'		,Units:'ml/min/g'	,Value:60D*P[0]/0.55	,Nr: 0} $
			,{Name:'Exponent (a)'	,Units:'1/sec'			,Value:1D*P[1]			,Nr: 1} $
			,{Name:'Amplitude (b)'	,Units:''				,Value:1D*P[2]			,Nr: 2} ]
	ENDIF


	Parameters = [Parameters,{Name:'Akaike Fit Error', Units:'', Value:AIC, Nr:9} ]
	IF Delay NE 0 THEN $
	Parameters = [Parameters,{Name:'Arterial Delay', Units:'sec', Value:1D*Pd, Nr:8} ]

	self.Curve[2] = ptr_new(Fit)
	self.Parameters = ptr_new(Parameters)
	Self->SET, OnDisplay=OnDisplay, /Sensitive
END




FUNCTION PMI__Display__CardiacSingleInletRoiSliderConc::Event, ev

	Uname = widget_info(ev.id,/uname)

	i = where(Uname Eq ['ROI','AIF']+'bttn', cnt)
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

	i = where(Uname Eq ['Model','Delay','Lower','Upper'], cnt)
	if cnt eq 1 then begin
		ptr_free, Self.Curve[2], self.parameters
	;	Self->SET, OnDisplay='ROI', /Refresh
		Self->SET, /Refresh
		return, 0B
	endif

	i = where(Uname Eq ['Export','Export As'], cnt)
	if cnt eq 1 then begin
		Self->GET, Time=Time, RoiCurve=RoiCurve, AifCurve=AifCurve, Fit=Fit, Model=Model, Delay=Delay, Roiname=Roiname, AifName=AifName
		if Uname eq 'Export' then begin
			PMI__Info, ev.top, Stdy=Stdy
			Path = Stdy->Datapath() + 'Cardiac Models (ROI)'
			file_mkdir, Path
			File = Path + '\' + Roiname + '_' + Model
			IF Delay  NE 0 THEN File=File+'_Delay'
		endif else begin
			PMI__Info, ev.top, State=State
			if not State -> Get_file(file, file=cleanstr(Roiname + '__' + Model), title='Save as..', filter='.tif') then return,0B
			File = strmid(file,0,strlen(file)-4)
		endelse
		Write_tiff, File + '.tif', reverse(tvrd(/true),3)
		PMI__WritePlot, File + '__Curve.txt', Time, RoiCurve
		PMI__WritePlot, File + '__Aif.txt', Time, AifCurve
		PMI__WritePlot, File + '__Fit.txt', time, Fit
		PMI__WritePlot, File + '__Parameters.txt' $
		, 	Xname = 'Parameters', (*self.Parameters).Name + '  (' + (*self.Parameters).Units + ')' $
		, 	Yname = 'Fitted Values', (*self.Parameters).Value
		return, 0B
	endif

	Display =  PMI__DisplayNew(ev.top, 'PMI__DISPLAY__2DVIEW')
	PMI__Control, ev.top, /refresh
	return, 0B
END

FUNCTION PMI__Display__Event__CardiacSingleInletRoiSliderConc, ev

	widget_control, ev.handler, get_uvalue=self
	return, Self -> Event(ev)
END



FUNCTION PMI__Display__CardiacSingleInletRoiSliderConc::GetCurve, Region

	Self -> GET, OnDisplay=OnDisplay, Time=Time, Stdy=Stdy, Region=Region
	Self -> SET, Message = 'Loading ' + Region->Name() + ' curve', Sensitive=0
	Y = PMI__RoiCurve(Stdy->DataPath(), Self.Series, Region, cnt=cnt)
	Self -> SET, /Erase, OnDisplay=OnDisplay, /Sensitive
	if cnt eq 0 then return, Time*0
	return, Y
END

FUNCTION PMI__Display__CardiacSingleInletRoiSliderConc::GetName, Region

	self->GET, Region=Region
	return, Region -> Name()
END


PRO PMI__Display__CardiacSingleInletRoiSliderConc::GET, $
 	CursorPos = CursorPos, $
	Model=Model, Delay=Delay, $
	ytitle=ytitle, OnDisplay=OnDisplay, $
	Time=Time, Fit=Fit, Indices=Indices, $
 	RoiCurve=RoiCurve, AifCurve=AifCurve, $
	RoiName=RoiName, AifName=AifName, $
	Region = Region, Stdy=Stdy

	if arg_present(CursorPos) then CursorPos=self.CursorPos

	if arg_present(Model) then begin
		list = widget_info(self.id,find_by_uname='Model')
		widget_control, list, Get_Value=Models
		Model = Models[widget_info(list,/droplist_select)]
	endif
	if arg_present(Delay) then Delay = widget_info(widget_info(self.id,find_by_uname='Delay'),/Button_set)
	if arg_present(ytitle) then ytitle='Concentration (mM)'
	if arg_present(Time) then begin
		t = Self.Series -> c(1)
		Time = t-t[0]
	endif

	if arg_present(RoiName) then RoiName=Self->GetName('ROI')
	if arg_present(AifName) then AifName=Self->GetName('AIF')

	if arg_present(RoiCurve) then $
		if ptr_valid(Self.Curve[0]) then RoiCurve = *Self.Curve[0] $
		else begin
			c = *self.constants
			RoiCurve = Self->GetCurve('ROI')
			S0 = total(RoiCurve[0:self.baseline-1])/self.baseline ;calculate the baseline
			RoiCurve = Concentration_SR_SpGRE(RoiCurve, S0, c.T1b, c.relaxivity, c.TD, c.FA, c.TR, c.Nphase)
			Self.Curve[0] = ptr_new(RoiCurve)
		endelse

	if arg_present(AifCurve) then $
		if ptr_valid(Self.Curve[1]) then AifCurve = *Self.Curve[1] $
		else begin
		    c = *self.constants
			AifCurve = Self->GetCurve('AIF')
			S0 = total(AifCurve[0:self.baseline-1])/self.baseline
			AifCurve = Concentration_SR_SpGRE(AifCurve, S0, c.T1b, c.relaxivity, c.TD, c.FA, c.TR, c.Nphase)/c.Hct
			Self.Curve[1] = ptr_new(AifCurve)
		endelse

	if arg_present(Indices) then begin
		widget_control, widget_info(self.id,find_by_uname='Upper'), get_value=i1
		widget_control, widget_info(self.id,find_by_uname='Lower'), get_value=i0
		Indices=[i0,i1]
	endif

	if arg_present(FIT) then begin
		if not ptr_valid(Self.Curve[2]) then Self->Fit
		FIT = *Self.Curve[2]
	endif

	if arg_present(OnDisplay) then begin

		if widget_info(widget_info(self.id,find_by_uname='ROIbttn'),/sensitive) then OnDisplay='AIF'
		if widget_info(widget_info(self.id,find_by_uname='AIFbttn'),/sensitive) then OnDisplay='ROI'
	endif

	if arg_present(Stdy) then PMI__Info, tlb(self.id), Stdy=Stdy

	if n_elements(Region) ne 0 then begin
		Self->GET, Stdy=Stdy
		Region = Stdy->Obj(1,widget_info(widget_info(self.id,find_by_uname=Region),/droplist_select))
	endif

END



PRO PMI__Display__CardiacSingleInletRoiSliderConc::SET, $
	PMI__REFRESH=pmi__refresh, PMI__RESIZE=pmi_resize, $
	Refresh=Refresh, Erase=Erase, $
	Message=Message, Sensitive=Sensitive, $
	xsize=xsize, ysize=ysize, $
	OnDisplay=OnDisplay

	if keyword_set(pmi_resize) then begin
		gpmi = widget_info(/geometry, tlb(self.id))
		self -> Set, xsize=gpmi.xsize, ysize=gpmi.ysize, /REFRESH
	endif

	if n_elements(Message) 	ne 0 then begin
		Self -> SET, /Erase
		xyouts, 0.1,0.9, Message, color=0,/normal,charsize=1.5,charthick=2.0
	endif
	if n_elements(Sensitive) ne 0 then widget_control, self.id, sensitive=sensitive

	if n_elements(xsize) ne 0 $
	or n_elements(ysize) ne 0 then begin
		widget_control, self.DrawId, xsize=xsize, ysize=ysize-40
		xs = floor((xsize - 665)/3E)
		if xs lt 30 then xs = 30
		List = ['AIF','ROI','Model']
		for i=0,2 do widget_control, widget_info(self.id,find_by_uname=List[i]), xsize=xs
	endif

	if keyword_set(Erase) then begin
		widget_control, self.DrawId, get_value = win
		wset, win & erase, 255
	endif

	if n_elements(OnDisplay) ne 0 then begin

		widget_control, widget_info(self.id,find_by_uname='AIFbttn'), Sensitive='AIF' NE OnDisplay
		widget_control, widget_info(self.id,find_by_uname='ROIbttn'), Sensitive='ROI' NE OnDisplay
 		widget_control, widget_info(self.id,find_by_uname='Export')		, Sensitive='ROI' EQ OnDisplay
		widget_control, widget_info(self.id,find_by_uname='Export As')	, Sensitive='ROI' EQ OnDisplay
	endif

	IF keyword_set(Refresh) THEN BEGIN

		Self->GET, OnDisplay=OnDisplay
		top=0.9 & dy=0.04 & x0=0.525 & charsize=1.5 & charthick=1.5

		CASE OnDisplay OF

		'AIF':begin
			Self -> GET, Time=Time, AifCurve=Curve, AifName=AifName, Indices=Ind, ytitle=ytitle
			Self -> SET, /Erase

			plot, /nodata, position=[0.1,0.2,0.5,0.9]  $
			, 	[0,max(time)], [min(Curve),max(Curve)] $
			, 	/xstyle, /ystyle $
			, 	background=255, color=0 $
			, 	xtitle = 'Time (sec)', ytitle=ytitle $
			, 	charsize=1.5, charthick=2.0, xthick=2.0, ythick=2.0
			oplot, time, Curve, color=14*16, psym=4, thick=2
			xyouts, x0, top-1*dy, 'Arterial Input Function: ' + AifName	, color=0, /normal, charsize=1.5, charthick=1.5

			IF Ind[0] LT Ind[1] THEN BEGIN
				oplot, time[Ind[0]:Ind[1]], Curve[Ind[0]:Ind[1]], psym=4, thick=2, color=0*16
				oplot, [time[Ind[0]],time[Ind[0]]], [min(Curve),max(Curve)], linestyle=0, thick=2, color=0*16
				oplot, [time[Ind[1]],time[Ind[1]]], [min(Curve),max(Curve)], linestyle=0, thick=2, color=0*16
			ENDIF
			end

		'ROI':BEGIN
			Self -> GET, RoiCurve=Curve, Time=Time, RoiName=RoiName, AifName=AifName, Indices=Ind, Model=Model, ytitle=ytitle
			IF Ind[0] LT Ind[1] THEN Self -> GET, Fit=Fit

			plot, /nodata, position=[0.1,0.2,0.5,0.9]  $
			, 	[0,max(time)], [min(Curve),max(Curve)] $
			, 	/xstyle, /ystyle $
			, 	background=255, color=0 $
			, 	xtitle = 'Time (sec)', ytitle=ytitle $
			, 	charsize=1.5, charthick=2.0, xthick=2.0, ythick=2.0
			oplot, time, Curve, color=14*16, psym=4, thick=2
			xyouts, x0, top-0*dy, 'Region Of Interest: ' + RoiName		, color=6*16, /normal, charsize=1.5, charthick=1.5
			xyouts, x0, top-1*dy, 'Arterial Input Function: ' + AifName	, color=0, /normal, charsize=1.5, charthick=1.5

			IF Ind[0] LT Ind[1] THEN BEGIN
				oplot, time[Ind[0]:Ind[1]], Curve[Ind[0]:Ind[1]], psym=4, thick=2, color=6*16
				oplot, [time[Ind[0]],time[Ind[0]]], [min(Curve),max(Curve)], linestyle=0, thick=2, color=0*16
				oplot, [time[Ind[1]],time[Ind[1]]], [min(Curve),max(Curve)], linestyle=0, thick=2, color=0*16
				oplot, time, Fit, color=12*16, linestyle=0, thick=2
				xyouts, x0, top-3*dy, 'Model: '+Model, color=12*16, /normal, charsize=1.5, charthick=1.5
				P = *self.Parameters
				for i=0L,n_elements(P)-1 do xyouts, $
					x0, top-dy*(5+P[i].Nr), $
					P[i].Name + ' = ' + PMI__Round(P[i].Value,3,/string) + ' ' + P[i].Units, $
					color=0, /normal, charsize=charsize, charthick=charthick
			ENDIF
			END
		ENDCASE
	ENDIF
END





PRO PMI__Display__CardiacSingleInletRoiSliderConc::Cleanup
	widget_control, self.id, /destroy
	ptr_free, Self.Curve, self.Parameters, self.Constants
	loadct, 0
END

FUNCTION PMI__Display__CardiacSingleInletRoiSliderConc::Init, parent, CursorPos, xsize=xsize, ysize=ysize, $
	Series = Series, $
	Baseline = nbase, $
	Constants = const, $
	set_droplist_select = sel

	self.CursorPos = CursorPos
	self.Series=series
	self.Baseline=nbase
	self.constants=ptr_new(const)

	loadct, 12

	PMI__Info, tlb(parent), Stdy=Stdy

	self.id = widget_base(parent,/column,event_func='PMI__Display__Event__CardiacSingleInletRoiSliderConc')
	Controls = widget_base(self.id,/row,ysize=40,/base_align_center,space=5)
	self.DrawId	= widget_draw(self.id,/retain)

		v = ['AIF','ROI']
		for i=0,1 do begin
			Base = widget_base(Controls,/row,/frame,/base_align_center)
			id = widget_button(Base, xsize=25, ysize=19, value=v[i], uname=v[i]+'bttn', sensitive=i-1)
  			id = widget_droplist(Base,/dynamic_resize, value=Stdy->Names(1), uname=v[i])
  			widget_control, id, Set_droplist_select=sel[i]
  		endfor

		Base = widget_base(Controls,/row,/frame,/base_align_center)
			id = widget_label(Base,value='Model')
  			id = widget_droplist(Base,/dynamic_resize, uname='Model',value = ['Fermi','Uptake','Compartment','Two-compartment'])
  			widget_control, id, set_droplist_select=1

		Base = widget_base(Controls,/row,/frame,/base_align_center)
			id = widget_slider(Base,/suppress_value,xsize=100,ysize=22,maximum=Series->d(3)-1,minimum=0,value=0,uname='Lower')
			id = widget_slider(Base,/suppress_value,xsize=100,ysize=22,maximum=Series->d(3)-1,minimum=0,value=Series->d(3)-1,uname='Upper')

		v = ['Delay']
		set_button = [1]
		for i=0,n_elements(v)-1 do begin
			Base = widget_base(Controls,/row,/frame,/nonexclusive)
			id = widget_button(Base, value=v[i], uname=v[i])
			widget_control, id, set_button=set_button[i]
		endfor

		v = ['Export','Export As','Done']
		Base = widget_base(Controls,/row,/frame,/base_align_center)
		for i=0,2 do id = widget_button(Base, xsize=50, ysize=22, value=v[i], uname=v[i])

	widget_control, self.id, set_uvalue = self
	self -> Set, xsize=xsize, ysize=ysize, /Refresh
	return, 1
END

PRO PMI__Display__CardiacSingleInletRoiSliderConc__Define

	SingleInletUptake
	SingleInletCompartment
	SingleInletExchange
	SingleInletFermi

	Struct = {PMI__Display__CardiacSingleInletRoiSliderConc, 	$
		id: 0L, 	$
		DrawId: 0L, $
		CursorPos:lonarr(4),	$
		Curve:ptrarr(3), $ ;RoiCurve, AifCurve, Fit
		Parameters: ptr_new(), $
		Series: obj_new(), $
		Baseline: 0L, $
		Constants: ptr_new() $
	}
END

pro PMI__Button__Event__CardiacSingleInletRoiSliderConc, ev

	PMI__Info, ev.top, Stdy=Stdy

    Series = Stdy->Names(0,ns,DefDim=3,ind=ind,sel=sel)
    Regions = Stdy->Names(1,nr)

	v = PMI__Form(ev.top, Title='Perfusion analysis setup', [$
		ptr_new({Type:'DROPLIST',Tag:'series', Label:'Dynamic series', Value:Series, Select:sel}), $
		ptr_new({Type:'DROPLIST',Tag:'aif'	 , Label:'Arterial Region', Value:Regions, Select:stdy->sel(1)}), $
		ptr_new({Type:'DROPLIST',Tag:'roi'	 , Label:'Tissue Region', Value:Regions, Select:stdy->sel(1)}), $
		ptr_new({Type:'VALUE'	,Tag:'nbase' , Label:'Length of baseline (# of dynamics)', Value:2L}), $
		ptr_new({Type:'VALUE'	,Tag:'T1b' , Label:'Blood T1 (msec)', Value:1725.0}), $
		ptr_new({Type:'VALUE'	,Tag:'T1t' , Label:'Tissue T1 (msec)', Value:1106.0})])
	IF v.cancel THEN return

	Hct = 0.55
	relaxivity = 3.8 ;Hz/mM
	TD = 100.0 ;msec
	TR = 1.85 ;msec
	FA = 11.0 ;deg
	Nphase = 36.

	Display = PMI__DisplayNew(ev.top, 'PMI__DISPLAY__CardiacSingleInletRoiSliderConc',$
		Series = Stdy->Obj(0,ind[v.series]), $
		Baseline = v.nbase, $
		Constants = {Hct:Hct, relaxivity:relaxivity, TD:TD, TR:TR, FA:FA, Nphase:Nphase, T1b:v.T1b, T1t:v.T1t}, $
		set_droplist_select = [v.aif,v.roi])

	PMI__Control, ev.top, MenuSensitive=0
end

pro PMI__Button__Control__CardiacSingleInletRoiSliderConc, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 1)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__CardiacSingleInletRoiSliderConc, parent,value=value, separator=separator

	if n_elements(value) eq 0 then value = 'Single-inlet models with variable time-window (ROI)'

	return, widget_button(parent, $
		value = value,	$
		event_pro = 'PMI__Button__Event__CardiacSingleInletRoiSliderConc',	$
		pro_set_value = 'PMI__Button__Control__CardiacSingleInletRoiSliderConc', $
	 	separator = separator )
end
