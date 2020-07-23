;17/04/2020: Added Default Field Strength of 7T (in case not specified in DICOM header)
;To accomdate data from Antaros

FUNCTION PMI__Display__tristanratsroi_v3_0::Constants

	FieldStrength = self.series->GETVALUE('0018'x,'0087'x)

	If FieldStrength Eq 0 then FieldStrength=7.0 ;ASK USER TO CHECK

	c = {rb:0E, rh:0E, ve:0E, vh:0E, ves:0E, R1l:0E, R1s:0E}

	; Relaxivity measurements provided by Claudia Green, Bayer, sept 2019

	Case floor(FieldStrength) of
		4.0: begin
			c.rb = 6.4 ;relaxivity of blood in Hz/mM ;assume spleen relaxivity is the same
			c.rh = 7.6 ;relaxivity of hepatocytes
			c.R1l = 1.285 ; per sec - liver R1 (Changed from 1.3203 on 23/07/2020)
			c.R1s = 0.631 ; per sec - spleen R1 (Changed from 0.7458 on 23/07/2020)
		end
		7.0: begin
			c.rb = 6.2 ;relaxivity of blood in Hz/mM ;assume spleen relaxivity is the same
			c.rh = 6.0 ;relaxivity of hepatocytes
			c.R1l = 0.8350 ; per sec - liver R1 (Changed from 0.8346 on 23/07/2020)
			c.R1s = 0.611 ; per sec - spleen R1 (Changed from 0.6313 on 23/07/2020)
		end
	Endcase

	;Volume values provided by Dan Scotcher (email 13 sept 2019)
	c.ve = 0.230 ;extracellular_volume_fraction in the liver
	c.vh = 0.722 ;hepatocyte_volume_fraction
	c.ves = 0.314 ;spleen_extracellular_volume_fraction

	return, c

END

PRO PMI__Display__tristanratsroi_v3_0::Fit

	Self->GET, Model=Model, Time=Time, AifCurve=Aif, RoiCurve=Curve, Indices=ti
	Self->SET, Message='Fitting...', Sensitive=0

	c = self->constants()
	aif /= c.rb ;spleen concentration
	aif /= c.ves ;extracellular concentration

	;FIT CURVE

	n = n_elements(Time)
	ti = *(self.Indices[0])
	ni = n_elements(ti)

	Pars = [0.0393, 0.00153] * (1-0.23) 	;[khe, kbh] in units 1/s
	parinfo = replicate({limited:[1B,0B], limits:[0D,1D]}, 2)
	Fit = mpcurvefit([c.rb,c.rh,c.ve,c.vh,ni,ti,Time,aif], $
		Curve, 1+0E*Curve, Pars, $
		function_name='TristanRatModel_v2_0', $
		/quiet,/NODERIVATIVE, PARINFO=parinfo)
	FitError = sqrt(total((Fit-Curve)^2))/sqrt(total((Curve)^2))
	if ni ne n then TristanRatModel_v2_0, [c.rb,c.rh,c.ve,c.vh, n,lindgen(n),Time,aif], Pars, Fit

	Parameters = $
		[{Name:'Hepatocellular Uptake Rate (khe) '		,Units:'ml/min/ml'		,Value: 60D*Pars[0]	        ,Nr: 2} $
		,{Name:'Biliary Efflux Rate (kbh) '		        ,Units:'ml/min/ml'		,Value: 60D*Pars[1]	        ,Nr: 3} $
		,{Name:'Hepatocellular Transit Time (Tc) '		,Units:'min'	        ,Value: c.vh/Pars[1]/60D	,Nr: 4} $
		,{Name:'RMS Fit Error '		                    ,Units:'%'	            ,Value: 100D*FitError	    ,Nr: 6} ]

	self.Curve[2] = ptr_new(Fit)
	self.Parameters = ptr_new(Parameters)
	Self->SET, /Sensitive
END

PRO PMI__Display__tristanratsroi_v3_0::Plot

	Self->GET, OnDisplay=OnDisplay

	top=0.9 & dy=0.04 & x0=0.525 & charsize=1.0 & charthick=1.0

	CASE OnDisplay OF

		'ROI':begin
			Self -> GET, Time=Time, RoiCurve=Y, RoiName=RoiName, Indices=Ind
			Self -> SET, /Erase
 			plot, /nodata, position=[0.1,0.2,0.5,0.9]  $
			, 	[0,max(time)], [min(Y),max(Y)] $
			, 	/xstyle, /ystyle $
			, 	background=255, color=0 $
			, 	xtitle = 'Time (sec)', ytitle= 'Change in R1 (1/sec)' $
			, 	charsize=1.5, charthick=2.0, xthick=2.0, ythick=2.0
			oplot, time[Ind], Y, color=6*16, linestyle=0, thick=2
			xyouts, x0, top-0*dy, 'Liver ROI: ' + RoiName, color=6*16, /normal, charsize=1.5, charthick=1.5
			end

		'AIF':begin
			Self -> GET, Time=Time, AifCurve=Y, AifName=AifName
			Self -> SET, /Erase
 			plot, time, Y, position=[0.1,0.2,0.5,0.9]  $
			, 	/xstyle, /ystyle $
			, 	background=255, color=0 $
			, 	xtitle = 'Time (sec)', ytitle='Change in R1 (1/sec)' $
			, 	linestyle=0, thick=2 $
			, 	charsize=1.5, charthick=2.0, xthick=2.0, ythick=2.0
			xyouts, x0, top-1*dy, 'Spleen ROI: ' + AifName	, color=0, /normal, charsize=1.5, charthick=1.5
			end

		'FIT':BEGIN
			Self -> GET, RoiCurve=Curve, AifCurve=Aif, Time=Time, Fit=Fit, Model=Model, RoiName=RoiName, AifName=AifName, Indices=Ind
			Self -> SET, /Erase

 			plot, /nodata, position=[0.1,0.2,0.5,0.9]  $
			, 	[0,max(time)], [min([min(Curve),min(Aif),min(Fit)]),max([max(Curve),max(Aif),max(Fit)])] $
			, 	/xstyle, /ystyle $
			, 	background=255, color=0 $
			, 	xtitle = 'Time (sec)', ytitle='Change in R1 (1/sec)' $
			, 	charsize=1.5, charthick=2.0, xthick=2.0, ythick=2.0
			oplot, time[Ind], Curve, color=6*16, psym=4, thick=2
			oplot, time, Aif, color=0*16, linestyle=0, thick=2
			oplot, time, Fit, color=12*16, linestyle=0, thick=2

			xyouts, x0, top-0*dy, 'Liver ROI: ' + RoiName	, color=6*16 , /normal, charsize=1.5, charthick=1.5
			xyouts, x0, top-1*dy, 'Spleen ROI: ' + AifName	, color=0    , /normal, charsize=1.5, charthick=1.5
			xyouts, x0, top-3*dy, 'Tissue Model: ' + Model	, color=12*16, /normal, charsize=1.5, charthick=1.5

			P = *self.Parameters

			for i=0L,n_elements(P)-1 do xyouts $
				, x0, top-dy*(5+P[i].Nr) $
				, P[i].Name + ' = ' + PMI__Round(P[i].Value,3,/string) + ' ' + P[i].Units $
				, color=0, /normal, charsize=charsize, charthick=charthick
			END
	ENDCASE

END




FUNCTION PMI__Display__tristanratsroi_v3_0::Event, ev

	Uname = widget_info(ev.id,/uname)

	if Uname Eq 'FITbttn' then begin
		widget_control, ev.id, sensitive=0
		widget_control, widget_info(self.id,find_by_uname='AIFbttn'), sensitive=1
		widget_control, widget_info(self.id,find_by_uname='ROIbttn'), sensitive=1
		list = widget_info(self.id,find_by_uname='FIT')
		widget_control, list, sensitive=1
		widget_control, widget_info(self.id,find_by_uname='Delay'), sensitive = widget_info(list,/droplist_select) GT 1
		self->plot
		return, 0B
	endif

	if Uname Eq 'ROIbttn' then begin
		widget_control, ev.id, sensitive=0
		widget_control, widget_info(self.id,find_by_uname='AIFbttn'), sensitive=1
		widget_control, widget_info(self.id,find_by_uname='FITbttn'), sensitive=1
		widget_control, widget_info(self.id,find_by_uname='FIT'), sensitive=0
		widget_control, widget_info(self.id,find_by_uname='Delay'), sensitive=0
		self->plot
		return, 0B
	endif

	if Uname Eq 'AIFbttn' then begin
		widget_control, ev.id, sensitive=0
		widget_control, widget_info(self.id,find_by_uname='ROIbttn'), sensitive=1
		widget_control, widget_info(self.id,find_by_uname='FITbttn'), sensitive=1
		widget_control, widget_info(self.id,find_by_uname='FIT'), sensitive=0
		widget_control, widget_info(self.id,find_by_uname='Delay'), sensitive=0
		self->plot
		return, 0B
	endif

	i = where(Uname Eq ['ROI','AIF'], cnt)
	If cnt eq 1 then begin
		ptr_free, Self.Curve[i], Self.Curve[2], self.parameters
		if i eq 0 then ptr_free, self.Indices[i]
		self->plot
		return, 0B
	endif

	if Uname Eq 'FIT' then begin
		widget_control, widget_info(self.id,find_by_uname='Delay'), sensitive=widget_info(ev.id,/droplist_select) GT 1
		ptr_free, Self.Curve[2], self.parameters
		self -> Plot
		return, 0B
	endif

	i = where(Uname Eq ['SIG','Delay'], cnt)
	if cnt eq 1 then begin
		ptr_free, Self.Curve[2], self.parameters
		self -> Plot
		return, 0B
	endif

	i = where(Uname Eq ['Export','Export As'], cnt)
	if cnt eq 1 then begin

		Self->GET, Time=Time, RoiCurve=RoiCurve, AifCurve=AifCurve, Fit=Fit, Model=Model, Roiname=Roiname, Indices=Ind
		if Uname eq 'Export' then begin
			PMI__Info, ev.top, Stdy=Stdy
			Path = Stdy->Datapath() + 'TRISTAN Rat Model v2.0 (ROI)'
			file_mkdir, Path
			File = Path + '\' + Roiname + '__SI_' + Model
		endif else begin
			PMI__Info, ev.top, State=State
			if not State -> Get_file(file, file=cleanstr(Roiname + '__' + Model), title='Save as..', filter='.tif') then return,0B
			File = strmid(file,0,strlen(file)-4)
		endelse
		Write_tiff, File + '.tif', reverse(tvrd(/true),3)

		ExportCurves = strarr(3,1+n_elements(time))
		ExportCurves[*,0] = ['Time (s)','Spleen Delta R1 (/sec)', 'Liver Delta R1 (/sec)']
		ExportCurves[0,1:*] = strcompress(Time,/remove_all)
		ExportCurves[1,1:*] = strcompress(AifCurve,/remove_all)
		ExportCurves[2,1+Ind] = strcompress(RoiCurve,/remove_all)
		PMI__Write_csv, File +'__Relaxation_rates.csv', ExportCurves

		ExportCurves = strarr(3,1+n_elements(time))
		ExportCurves[*,0] = ['Time (s)','Spleen (a.u.)', 'Liver (a.u.)']
		ExportCurves[0,1:*] = strcompress(Time,/remove_all)
		ExportCurves[1,1:*] = strcompress(*Self.Curve[1],/remove_all)
		ExportCurves[2,1+Ind] = strcompress(*Self.Curve[0],/remove_all)
		PMI__Write_csv, File +'__Signals.csv', ExportCurves

		ExportParameters = strarr(3,4)
		ExportParameters[0,*] = (*self.Parameters).Name + '  (' + (*self.Parameters).Units + ')'
		ExportParameters[1,*] = strcompress((*self.Parameters).Value,/remove_all)
		PMI__Write_csv, File + '__Parameters.csv', ExportParameters

		return, 0B
	endif

	Menu = widget_info(widget_info(ev.top,/child),/all_children)
	for i=0L,n_elements(Menu)-1 do widget_control, Menu[i], /sensitive
	PMI__Control, ev.top, Viewer = 'PMI__DISPLAY__2DVIEW'
	PMI__Control, ev.top, /refresh
	return, 0B
END

FUNCTION PMI__Display__Event__tristanratsroi_v3_0, ev

	widget_control, ev.handler, get_uvalue=self
	return, Self -> Event(ev)
END

FUNCTION PMI__Display__tristanratsroi_v3_0::DR1, Region

	case Region of
		'ROI':Signal=*Self.Curve[0]
		'AIF':Signal=*Self.Curve[1]
	endcase

	S0 = total(Signal[0:self.baseline-1])/self.baseline

	IF self.TR EQ 0 THEN BEGIN
		self.TR = self.series->GETVALUE('0018'x,'0080'x)
		self.FA = 20.0; self.series->GETVALUE('0018'x,'1314'x)
	ENDIF

	c = self->constants()
	case Region of
		'ROI':return, Concentration_SPGRESS(Signal, S0, 1000/c.R1l, self.FA, self.TR, 1.0) ;change in R1 (1/sec)
		'AIF':return, Concentration_SPGRESS(Signal, S0, 1000/c.R1s, self.FA, self.TR, 1.0) ;change in R1 (1/sec)
	endcase

END

FUNCTION PMI__Display__tristanratsroi_v3_0::GetCurve, Region, INDICES=TimeIndices

	Self -> GET, Time=Time, Stdy=Stdy, Region=Region
	Self -> SET, Message = 'Loading ' + Region->Name() + ' curve', Sensitive=0
	Signal = PMI__RoiCurve(Stdy->DataPath(), Self.Series, Region, INDICES=TimeIndices, cnt=cnt)
	Self -> SET, /Sensitive
	if cnt eq 0 then begin
		TimeIndices = lindgen(n_elements(Time))
		return, Time*0
	endif
	return, Signal
END

FUNCTION PMI__Display__tristanratsroi_v3_0::GetName, Region

	self->GET, Region=Region
	return, Region -> Name()
END


PRO PMI__Display__tristanratsroi_v3_0::GET, $
 	CursorPos = CursorPos, $
	Model=Model, Delay=Delay, $
	Time=Time, Fit=Fit, SignalModel=SignalModel, $
 	RoiCurve=RoiCurve, AifCurve=AifCurve, Indices=RoiIndices, $
	RoiName=RoiName, AifName=AifName, $
 	OnDisplay=OnDisplay, $
	RoiVolume=RoiVolume, $
	Region = Region, Stdy=Stdy

	if arg_present(CursorPos) then CursorPos=self.CursorPos

	if arg_present(Model) then begin
		list = widget_info(self.id,find_by_uname='FIT')
		widget_control, list, Get_Value=Models
		Model = Models[widget_info(list,/droplist_select)]
	endif

	if arg_present(Delay) then $
		Delay = widget_info(widget_info(self.id,find_by_uname='Delay'),/Button_set)

	if arg_present(Time) then begin
		t = Self.Series -> c(1)
		Time = t-t[0]
	endif

	if arg_present(RoiName) then RoiName=Self->GetName('ROI')
	if arg_present(AifName) then AifName=Self->GetName('AIF')

	if arg_present(RoiCurve) then begin
		if not ptr_valid(Self.Curve[0]) then begin
			Curve = Self->GetCurve('ROI', INDICES=RoiIndices)
			Self.Curve[0] = ptr_new(Curve)
			Self.Indices[0] = ptr_new(RoiIndices)
		endif else RoiIndices = *(Self.Indices[0])
		RoiCurve = self->DR1('ROI')
	endif

	if arg_present(AifCurve) then begin
		if not ptr_valid(Self.Curve[1]) then begin
			Curve = Self->GetCurve('AIF', INDICES=AifIndices)
			Self -> GET, Time=Time
			if n_elements(AifIndices) LT n_elements(Time) then $
				Curve = INTERPOL(Curve, Time[AifIndices], Time)
			Self.Curve[1] = ptr_new(Curve)
		endif
		AifCurve = Self->DR1('AIF')
	endif

	if arg_present(FIT) then begin
		if not ptr_valid(Self.Curve[2]) then Self->Fit
		FIT = *Self.Curve[2]
		endif

	if arg_present(SignalModel) then begin
		id = widget_info(self.id,find_by_uname='SIG')
		SignalModel = widget_info(id,/droplist_select)
	endif

	if arg_present(OnDisplay) then begin
		OnDisplay = ''
		List = ['AIF','ROI','FIT']
		for i=0L,2 do $
		if 0 eq widget_info(widget_info(self.id,find_by_uname=List[i]+'bttn'),/sensitive) then OnDisplay=List[i]
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
		thick = self.Series->GetValue('0018'x,'0088'x)
		pixel = self.Series->GetValue('0028'x,'0030'x)
		RoiVolume = npix*(thick[0]/10D)*(pixel[0]/10D)^2
	endif
END

PRO PMI__Display__tristanratsroi_v3_0::SET, $
	PMI__REFRESH=pmi__refresh, PMI__RESIZE=pmi_resize, $
	Refresh=Refresh, Erase=Erase, $
	Message=Message, Sensitive=Sensitive, $
	xsize=xsize, ysize=ysize, $
	Set_droplist_select = Set_droplist_select, $
	Series=Series, Baseline=Baseline, $
	OnDisplay=OnDisplay

	if keyword_set(pmi_resize) then begin
		gpmi = widget_info(/geometry, tlb(self.id))
		self -> Set, xsize=gpmi.xsize, ysize=gpmi.ysize
		self->plot
	endif

	if n_elements(Series) 	ne 0 then Self.Series = Series
	if n_elements(Baseline) 	ne 0 then self.Baseline = Baseline
	if n_elements(Message) 	ne 0 then begin
		Self->Set, /Erase
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
		List = ['AIF','ROI']
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





PRO PMI__Display__tristanratsroi_v3_0::Cleanup
	widget_control, self.id, /destroy
	ptr_free, Self.Curve, self.Indices, self.Parameters
	loadct, 0
END

FUNCTION PMI__Display__tristanratsroi_v3_0::Init, parent, CursorPos, xsize=xsize, ysize=ysize

	if n_elements(CursorPos) ne 0 then self.CursorPos = CursorPos

	loadct, 12

	PMI__Info, tlb(parent), Stdy=Stdy

	self.id = widget_base(parent,/column,map=0,event_func='PMI__Display__Event__tristanratsroi_v3_0')
	Controls = widget_base(self.id,/row,ysize=40,/base_align_center,space=5)
	self.DrawId	= widget_draw(self.id,/retain)

		v = ['ROI','AIF']
		for i=0,1 do begin
			Base = widget_base(Controls,/row,/frame,/base_align_center)
			id = widget_button(Base, xsize=25, ysize=19, value=v[i], uname=v[i]+'bttn')
  			id = widget_droplist(Base,/dynamic_resize, value=Stdy->Names(1), uname=v[i])
  		endfor

		Base = widget_base(Controls,/row,/frame,/base_align_center)
			id = widget_droplist(Base,/dynamic_resize, uname='SIG',value = ['Spoiled GRE (mM)'])
  			widget_control, id, set_droplist_select = 0, sensitive=0

		Base = widget_base(Controls,/row,/frame,/base_align_center)
			id = widget_button(Base, xsize=25, ysize=19, value='FIT', uname='FITbttn')
			id = widget_droplist(Base,/dynamic_resize, uname='FIT',value = ['TRISTAN Rat Model v2.0'])
  			widget_control, id, set_droplist_select = 0, sensitive=0

		Base = widget_base(Controls,/row,/frame,/nonexclusive)
		id = widget_button(Base, value='Delay', uname='Delay')
		widget_control, id, sensitive = 0

		v = ['Export','Export As','Close']
		Base = widget_base(Controls,/row,/frame,/base_align_center)
		for i=0,2 do id = widget_button(Base, xsize=50, ysize=22, value=v[i], uname=v[i])

	self -> Set, xsize=xsize, ysize=ysize, OnDisplay='FIT'
	widget_control, self.id, set_uvalue = self, /map
	return, 1
END

PRO PMI__Display__tristanratsroi_v3_0__Define

	TristanRatModel_v2_0

	Struct = {PMI__Display__tristanratsroi_v3_0 	$
	,	id: 0L 	$
	,	DrawId: 0L $
	,	CursorPos:lonarr(4)	$
	,	Curve:ptrarr(3) $ ;RoiCurve, AifCurve, Fit
	,	Indices:ptrarr(1) $ ; RoiIndices
	,	Parameters: ptr_new() $
	,	Series: obj_new() $
	,	Baseline: 0L $
	,	TR: 0E $
	,	FA: 0E $
	}
END


pro PMI__Button__Event__tristanratsroi_v3_0, ev

	PMI__Info, ev.top, Stdy=Stdy

    Series = Stdy->Names(0,ns,DefDim=3,ind=ind,sel=sel)
    Regions = Stdy->Names(1,nr)

	v = PMI__Form(ev.top, Title='Perfusion analysis setup', [$
		ptr_new({Type:'DROPLIST',Tag:'series', Label:'Dynamic series', Value:Series, Select:sel}), $
		ptr_new({Type:'DROPLIST',Tag:'aif'	 , Label:'Spleen Region', Value:Regions, Select:stdy->sel(1)}), $
		ptr_new({Type:'DROPLIST',Tag:'roi'	 , Label:'Liver Region', Value:Regions, Select:stdy->sel(1)}), $
		ptr_new({Type:'VALUE'	,Tag:'nbase' , Label:'Number of precontrast scans', Value:5L})])
	IF v.cancel THEN return

	PMI__Control, ev.top, Viewer = 'PMI__Display__tristanratsroi_v3_0', Display=Display

	Display -> Set, /Refresh, $
		Series = Stdy->Obj(0,ind[v.series]), $
		Baseline = v.nbase, $
		set_droplist_select = [v.roi,v.aif]
end

pro PMI__Button__Control__tristanratsroi_v3_0, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 1)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__tristanratsroi_v3_0, parent,value=value, separator=separator

	PMI__Display__tristanratsroi_v3_0__Define

	if n_elements(value) eq 0 then value = 'TRISTAN RAT models (ROI)'

	return, widget_button(parent, $
		value = value,	$
		event_pro = 'PMI__Button__Event__tristanratsroi_v3_0',	$
		pro_set_value = 'PMI__Button__Control__tristanratsroi_v3_0', $
	 	separator = separator )
end
