PRO PMI__Display__ToftsKidneyModelROI::Fit

	Self->GET, Model=Model, Efflux=Efflux, Delay=Delay, Time=Time, AifCurve=Aif, RoiCurve=Curve, Indices=ind, OnDisplay=OnDisplay, RoiVolume=Vol
	Self->SET, Sensitive=0

	IF Delay NE 0 THEN Steps = [0,5,0.1]

	ti = ind[0]+lindgen(1+ind[1]-ind[0])

	IF Model EQ 'Delta Function' AND Efflux EQ 0 THEN BEGIN

		P = [0.1, 30.0/6000] ;[VP, FE]
		Fit = FitSingleInlet('Patlak',time,aif,curve,P, INDICES=ti, $
			DELAY_PAR=Pd, DELAY_VALUES=steps, AKAIKE_ERROR=aic, /POSITIVITY, LIMITED_ABOVE=[1,0] )
		Parameters = $
			[{Name:'Blood Volume'		,Units:'%'		,Value:100D*P[0]/(1-self.Hematocrit)	,Nr: 1} $
			,{Name:'Ktrans'				,Units:'/min'	,Value:60D*P[1]	,Nr: 2}$
			,{Name:'ROI GFR'			,Units:'ml/min'	,Value:Vol*60D*P[1]	,Nr: 3}]
	ENDIF

	IF Model EQ 'Delta Function' AND Efflux EQ 1 THEN BEGIN

		P = [0.3, 2.0/3, 12.0/6000] 	;[VP+VTapp, VTapp/(VP+VTapp), FE]
		Fit = FitSingleInlet('ModifiedTofts',time,aif,curve,P, INDICES=ti, $
			DELAY_PAR=Pd, DELAY_VALUES=steps, AKAIKE_ERROR=aic, /POSITIVITY, LIMITED_ABOVE=[0,1,0])
		Parameters = $
			[{Name:'Blood Volume'	,Units:'%'			,Value:100D*P[0]*(1-P[1])/(1-self.Hematocrit)	,Nr: 1} $
			,{Name:'Ktrans'			,Units:'/min'		,Value:60D*P[2]	,Nr: 2}$
			,{Name:'ROI GFR'		,Units:'ml/min'		,Value:Vol*60D*P[2]	,Nr: 3} ]
	ENDIF

	IF Model EQ 'Exponential' AND Efflux EQ 0 THEN BEGIN

		P = [0.1, 120.0/6000, 0.1] 	;[VP, FP, FT/FP]
		Fit = FitSingleInlet('2CUptakeFiltration',time,aif,curve,P, INDICES=ti, $
			DELAY_PAR=Pd, DELAY_VALUES=steps, AKAIKE_ERROR=aic, /POSITIVITY, LIMITED_ABOVE=[1,0,1])
		Parameters = $
			[{Name:'Blood Flow'		,Units:'ml/100ml/min'	,Value:6000D*P[1]/(1-self.Hematocrit)	,Nr: 0} $
			,{Name:'Blood Volume'	,Units:'%'		,Value:100D*P[0]/(1-self.Hematocrit)	,Nr: 1} $
			,{Name:'Ktrans'			,Units:'/min'	,Value:60D*P[2]*P[1]		,Nr: 2} $
			,{Name:'ROI GFR'		,Units:'ml/min'	,Value:Vol*60D*P[2]*P[1]	,Nr: 3} ]
	ENDIF

	IF Model EQ 'Exponential' AND Efflux EQ 1 THEN BEGIN

		P = [0.3, 120.0/6000, 2.0/3, 0.1] ;[VP+VTapp, FP, VTapp/(VP+VTapp), FT/FP]
		Fit = FitSingleInlet('Filtration',time,aif,curve,P, INDICES=ti, $
			DELAY_PAR=Pd, DELAY_VALUES=steps, AKAIKE_ERROR=aic, /POSITIVITY, LIMITED_ABOVE=[0,0,1,1])
   		Parameters = $
			[{Name:'Blood Flow'		,Units:'ml/100ml/min'	,Value:6000D*P[1]/(1-self.Hematocrit),Nr: 0} $
			,{Name:'Blood Volume'	,Units:'ml/100ml'		,Value:100.0*P[0]*(1-P[2])/(1-self.Hematocrit),Nr: 1} $
			,{Name:'Ktrans'			,Units:'/min'			,Value:60D*P[1]*P[3]		,Nr: 2} $
			,{Name:'ROI GFR'		,Units:'ml/min'			,Value:Vol*60D*P[1]*P[3]	,Nr: 3} ]
	ENDIF

	Parameters = [Parameters,{Name:'Akaike Fit Error', Units:'', Value:AIC, Nr:6} ]
	IF Delay NE 0 THEN $
	Parameters = [Parameters,{Name:'Arterial Delay', Units:'sec', Value:1D*Pd, Nr:4} ]

	self.Curve[2] = ptr_new(Fit)
	self.Parameters = ptr_new(Parameters)
	Self->SET, OnDisplay=OnDisplay, /Sensitive
END




FUNCTION PMI__Display__ToftsKidneyModelROI::Event, ev

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

	i = where(Uname Eq ['GIRF','Delay','Efflux','Lower','Upper'], cnt)
	if cnt eq 1 then begin
		ptr_free, Self.Curve[2], self.parameters
		Self->SET, OnDisplay='ROI', /Refresh
		return, 0B
	endif

	i = where(Uname Eq ['Export','Export As'], cnt)
	if cnt eq 1 then begin
		Self->GET, Time=Time, RoiCurve=RoiCurve, AifCurve=AifCurve, Fit=Fit, Model=Model, Efflux=Efflux, Delay=Delay, Roiname=Roiname, AifName=AifName
		if Uname eq 'Export' then begin
			PMI__Info, ev.top, Stdy=Stdy
			Path = Stdy->Datapath() + 'Tofts Kidney Model (ROI)'
			file_mkdir, Path
			File = Path + '\' + Roiname + '_' + Model
			IF Efflux NE 0 THEN File=File+'_Efflux'
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

FUNCTION PMI__Display__Event__ToftsKidneyModelROI, ev

	widget_control, ev.handler, get_uvalue=self
	return, Self -> Event(ev)
END



FUNCTION PMI__Display__ToftsKidneyModelROI::GetCurve, Region

	Self -> GET, OnDisplay=OnDisplay, Time=Time, Stdy=Stdy, Region=Region
	Self -> SET, Message = 'Loading ' + Region->Name() + ' curve', Sensitive=0
	Y = PMI__RoiCurve(Stdy->DataPath(), Self.Series, Region, cnt=cnt)
	Self -> SET, /Erase, OnDisplay=OnDisplay, /Sensitive
	if cnt eq 0 then return, Time*0
	return, Y
END

FUNCTION PMI__Display__ToftsKidneyModelROI::GetName, Region

	self->GET, Region=Region
	return, Region -> Name()
END


PRO PMI__Display__ToftsKidneyModelROI::GET, $
 	CursorPos = CursorPos, $
	Model=Model, Efflux=Efflux, Delay=Delay, $
	Time=Time, Fit=Fit, Indices=Indices, $
 	RoiCurve=RoiCurve, AifCurve=AifCurve, $
	RoiName=RoiName, AifName=AifName, $
 	OnDisplay=OnDisplay, $
 	RoiVolume=RoiVolume, $
	Region = Region, $
	Stdy=Stdy

	if arg_present(CursorPos) then CursorPos=self.CursorPos

	if arg_present(Model) then begin
		list = widget_info(self.id,find_by_uname='GIRF')
		widget_control, list, Get_Value=Models
		Model = Models[widget_info(list,/droplist_select)]
	endif
	if arg_present(Efflux) then $
		Efflux = widget_info(widget_info(self.id,find_by_uname='Efflux'),/Button_set)
	if arg_present(Delay) then $
		Delay = widget_info(widget_info(self.id,find_by_uname='Delay'),/Button_set)

	if arg_present(Time) then begin
		t = Self.Series -> c(1)
		Time = t-t[0]
	endif

	if arg_present(RoiName) then RoiName=Self->GetName('ROI')
	if arg_present(AifName) then AifName=Self->GetName('AIF')

	if arg_present(RoiCurve) then $
		if ptr_valid(Self.Curve[0]) then RoiCurve = *Self.Curve[0] $
		else begin
			RoiCurve = Self->GetCurve('ROI')
			S0 = total(RoiCurve[0:self.baseline-1])/self.baseline ;calculate the baseline
			TR = self.series->GETVALUE('0018'x,'0080'x)
			FA = self.series->GETVALUE('0018'x,'1314'x)
			RoiCurve = Concentration_SPGRESS(RoiCurve, S0, 1300.0, FA, TR, 4.5)
			Self.Curve[0] = ptr_new(RoiCurve)
		endelse

	if arg_present(AifCurve) then $
		if ptr_valid(Self.Curve[1]) then AifCurve = *Self.Curve[1] $
		else begin
			AifCurve = Self->GetCurve('AIF')
			S0 = total(AifCurve[0:self.baseline-1])/self.baseline
			TR = self.series->GETVALUE('0018'x,'0080'x)
			FA = self.series->GETVALUE('0018'x,'1314'x)
			AifCurve = Concentration_SPGRESS(AifCurve, S0, 1400.0, FA, TR, 4.5)
			AifCurve = AifCurve/(1-self.Hematocrit)
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

	if arg_present(RoiVolume) then begin
		Region='ROI'
		Self->GET, Stdy=Stdy, Region=Region
		v = PMI__RoiValues(Stdy->DataPath(), self.series, Region, cnt=npix)
		thick = self.Series->GetValue('0018'x,'0050'x)
		pixel = self.Series->GetValue('0028'x,'0030'x)
		RoiVolume = npix*(thick/10E)*(pixel[0]/10E)^2
	endif

END



PRO PMI__Display__ToftsKidneyModelROI::SET, $
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
		List = ['AIF','ROI','GIRF']
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
			Self -> GET, Time=Time, AifCurve=Y, AifName=AifName
			Self -> SET, /Erase
 			plot, time, Y, position=[0.1,0.2,0.5,0.9]  $
			, 	/xstyle, /ystyle $
			, 	background=255, color=0 $
			, 	xtitle = 'Time (sec)', ytitle='Concentrations (mM)' $
			, 	linestyle=0, thick=2 $
			, 	charsize=1.5, charthick=2.0, xthick=2.0, ythick=2.0
			xyouts, x0, top-1*dy, 'Arterial Input Function: ' + AifName	, color=0, /normal, charsize=1.5, charthick=1.5
			end

		'ROI':BEGIN
			Self -> GET, RoiCurve=Curve, Time=Time, RoiName=RoiName, AifName=AifName, Indices=Ind, Model=Model, Efflux=Efflux
			IF Ind[0] LT Ind[1] THEN Self -> GET, Fit=Fit

			plot, /nodata, position=[0.1,0.2,0.5,0.9]  $
			, 	[0,max(time)], [min(Curve),max(Curve)] $
			, 	/xstyle, /ystyle $
			, 	background=255, color=0 $
			, 	xtitle = 'Time (sec)', ytitle='Concentrations (mM)' $
			, 	charsize=1.5, charthick=2.0, xthick=2.0, ythick=2.0
			oplot, time, Curve, color=14*16, linestyle=0, thick=2
			xyouts, x0, top-0*dy, 'Region Of Interest: ' + RoiName		, color=6*16, /normal, charsize=1.5, charthick=1.5
			xyouts, x0, top-1*dy, 'Arterial Input Function: ' + AifName	, color=0, /normal, charsize=1.5, charthick=1.5

			IF Ind[0] LT Ind[1] THEN BEGIN
				oplot, time[Ind[0]:Ind[1]], Curve[Ind[0]:Ind[1]], linestyle=0, thick=2, color=6*16
				oplot, [time[Ind[0]],time[Ind[0]]], [min(Curve),max(Curve)], linestyle=0, thick=2, color=0*16
				oplot, [time[Ind[1]],time[Ind[1]]], [min(Curve),max(Curve)], linestyle=0, thick=2, color=0*16
				oplot, time, Fit, color=12*16, linestyle=0, thick=2
				IF Efflux EQ 0 THEN Efflux='Off' ELSE Efflux='On'
				xyouts, x0, top-3*dy, 'GIRF: '+Model + ',  Efflux: '+Efflux, color=12*16, /normal, charsize=1.5, charthick=1.5
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





PRO PMI__Display__ToftsKidneyModelROI::Cleanup
	widget_control, self.id, /destroy
	ptr_free, Self.Curve, self.Parameters
	loadct, 0
END

FUNCTION PMI__Display__ToftsKidneyModelROI::Init, parent, CursorPos, xsize=xsize, ysize=ysize, $
	Series = Series, $
	Baseline = nbase, $
	Hematocrit = hct, $
	set_droplist_select = sel

	self.CursorPos = CursorPos
	self.Series=series
	self.Baseline=nbase
	self.Hematocrit=hct

	loadct, 12

	PMI__Info, tlb(parent), Stdy=Stdy

	self.id = widget_base(parent,/column,event_func='PMI__Display__Event__ToftsKidneyModelROI')
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
			id = widget_label(Base,value='GIRF')
  			id = widget_droplist(Base,/dynamic_resize, uname='GIRF',value = ['Delta Function','Exponential'])
  			widget_control, id, set_droplist_select=1

		Base = widget_base(Controls,/row,/frame,/base_align_center)
			id = widget_slider(Base,/suppress_value,xsize=100,ysize=22,maximum=Series->d(3)-1,minimum=0,value=0,uname='Lower')
			id = widget_slider(Base,/suppress_value,xsize=100,ysize=22,maximum=Series->d(3)-1,minimum=0,value=Series->d(3)-1,uname='Upper')

		v = ['Efflux','Delay']
		set_button = [1,0]
		for i=0,1 do begin
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

PRO PMI__Display__ToftsKidneyModelROI__Define

	SingleInletPatlak
	SingleInletModifiedTofts
	SingleInlet2CUptakeFiltration
	SingleInletFiltration

	Struct = {PMI__Display__ToftsKidneyModelROI, 	$
		id: 0L, 	$
		DrawId: 0L, $
		CursorPos:lonarr(4),	$
		Curve:ptrarr(3), $ ;RoiCurve, AifCurve, Fit
		Parameters: ptr_new(), $
		Series: obj_new(), $
		Baseline: 0L, $
		Hematocrit: 0E $
	}
END


