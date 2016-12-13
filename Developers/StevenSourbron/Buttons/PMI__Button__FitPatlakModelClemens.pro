PRO PMI__Display__FitPatlakModelClemens::Fit

	Self->GET, Time=Time, AifCurve=Aif, RoiCurve=Curve, OnDisplay=OnDisplay
	Self->SET, Message='Fitting...', Sensitive=0

	Fit = FitPatlakNonlinearDelay(time,curve,aif,Pars=P,init=5,/constrained,/nodelay,/mp,/noderivative,/quiet)
	Parameters = $
		[{Name:'Blood Volume'			,Units:'%'			,Value:100D*P[0]		,Nr: 1} $
		,{Name:'Albumin Permeability'	,Units:'/1000/min'	,Value:60*1000D*P[1]	,Nr: 2} ]

	self.Curve[2] = ptr_new(Fit)
	self.Parameters = ptr_new(Parameters)
	Self->SET, OnDisplay=OnDisplay, /Sensitive
END

PRO PMI__Display__FitPatlakModelClemens::Plot

	Self->GET, OnDisplay=OnDisplay

	top=0.9 & dy=0.04 & x0=0.525 & charsize=1.5 & charthick=1.5

	CASE OnDisplay OF

		'':Self->Set, /Erase

		'ROI':begin
			Self -> GET, Time=Time, RoiCurve=Y, RoiName=RoiName
			Self -> SET, /Erase
 			plot, /nodata, position=[0.1,0.2,0.5,0.9]  $
			, 	[0,max(time)], [min(Y),max(Y)] $
			, 	/xstyle, /ystyle $
			, 	background=255, color=0 $
			, 	xtitle = 'Time (sec)', ytitle='Signal Enhancement' $
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
			, 	xtitle = 'Time (sec)', ytitle='Signal Enhancement' $
			, 	linestyle=0, thick=2 $
			, 	charsize=1.5, charthick=2.0, xthick=2.0, ythick=2.0
			xyouts, x0, top-1*dy, 'Arterial Input Function: ' + AifName	, color=0, /normal, charsize=1.5, charthick=1.5
			end

		'FIT':BEGIN
			Self -> GET, RoiCurve=Curve, Time=Time, Fit=Fit, RoiName=RoiName, AifName=AifName
			Self -> SET, /Erase

 			plot, /nodata, position=[0.1,0.2,0.5,0.9]  $
			, 	[0,max(time)], [min([min(Curve),min(Fit)]),max([max(Curve),max(Fit)])] $
			, 	/xstyle, /ystyle $
			, 	background=255, color=0 $
			, 	xtitle = 'Time (sec)', ytitle='Signal Enhancement' $
			, 	charsize=1.5, charthick=2.0, xthick=2.0, ythick=2.0
			oplot, time, Curve, color=6*16, linestyle=0, thick=2
			oplot, time, Fit, color=12*16, linestyle=0, thick=2

			xyouts, x0, top-0*dy, 'Region Of Interest: ' + RoiName		, color=6*16, /normal, charsize=1.5, charthick=1.5
			xyouts, x0, top-1*dy, 'Arterial Input Function: ' + AifName	, color=0, /normal, charsize=1.5, charthick=1.5

			P = *self.Parameters

			for i=0L,n_elements(P)-1 do xyouts $
				, x0, top-dy*(5+P[i].Nr) $
				, P[i].Name + ' = ' + PMI__Round(P[i].Value,4,/string) + ' ' + P[i].Units $
				, color=0, /normal, charsize=charsize, charthick=charthick
			END
	ENDCASE

END




FUNCTION PMI__Display__FitPatlakModelClemens::Event, ev

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

	i = where(Uname Eq ['Export','Export As'], cnt)
	if cnt eq 1 then begin
		Self->GET, Time=Time, RoiCurve=RoiCurve, Fit=Fit, Roiname=Roiname
		if Uname eq 'Export' then begin
			PMI__Info, ev.top, Stdy=Stdy
			Path = Stdy->Datapath() + 'Patlak Model (Albumin)'
			file_mkdir, Path
			File = Path + '\' + Roiname + '__SI_Patlak'
		endif else begin
			PMI__Info, ev.top, State=State
			if not State -> Get_file(file, file=cleanstr(Roiname + '__Patlak'), title='Save as..', filter='.tif') then return,0B
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
	PMI__Control, ev.top, Viewer = 'PMI__DISPLAY__2DVIEW'
	PMI__Control, ev.top, /refresh
	return, 0B
END

FUNCTION PMI__Display__Event__FitPatlakModelClemens, ev

	widget_control, ev.handler, get_uvalue=self
	return, Self -> Event(ev)
END



FUNCTION PMI__Display__FitPatlakModelClemens::GetCurve, List

	PMI__Info, tlb(self.id), Stdy=Stdy
	Region = Stdy->Obj(1,widget_info(widget_info(self.id,find_by_uname=List),/droplist_select))
	Self -> GET, OnDisplay=OnDisplay, Time=Time
	Self -> SET, Message = 'Loading ' + Region->Name() + ' curve', Sensitive=0
	Y = PMI__RoiCurve(Stdy->DataPath(), Self.Series, Region, cnt=cnt)
	Self -> SET, OnDisplay=OnDisplay, /Sensitive
	if cnt eq 0 then return, Time*0
	return, LMU__Enhancement(Y,4,relative=0)
END

FUNCTION PMI__Display__FitPatlakModelClemens::GetName, List

	PMI__Info, tlb(self.id), Stdy=Stdy
	Region = Stdy -> Obj(1,widget_info(widget_info(self.id,find_by_uname=List),/droplist_select))
	return, Region -> Name()
END


PRO PMI__Display__FitPatlakModelClemens::GET, $
 	CursorPos = CursorPos, $
	Time=Time, Fit=Fit, $
 	RoiCurve=RoiCurve, AifCurve=AifCurve, $
	RoiName=RoiName, AifName=AifName, $
 	OnDisplay=OnDisplay

	if arg_present(CursorPos) then CursorPos=self.CursorPos

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
			Self.Curve[0] = ptr_new(RoiCurve)
		endelse

	if arg_present(AifCurve) then $
		if ptr_valid(Self.Curve[1]) then AifCurve = *Self.Curve[1] $
		else begin
			AifCurve = Self->GetCurve('AIF')
			Self.Curve[1] = ptr_new(AifCurve)
		endelse

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

PRO PMI__Display__FitPatlakModelClemens::SET, $
	PMI__REFRESH=pmi__refresh, PMI__RESIZE=pmi_resize, $
	Refresh=Refresh, Erase=Erase, $
	Message=Message, Sensitive=Sensitive, $
	xsize=xsize, ysize=ysize, $
	Set_droplist_select = Set_droplist_select, $
	Series=Series, $
	OnDisplay=OnDisplay

	if keyword_set(pmi_resize) then begin
		gpmi = widget_info(/geometry, tlb(self.id))
		self -> Set, xsize=gpmi.xsize, ysize=gpmi.ysize, /REFRESH
	endif

	if n_elements(Series) 	ne 0 then Self.Series = Series

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
		xs = floor((xsize - 360)/2E)
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





PRO PMI__Display__FitPatlakModelClemens::Cleanup
	widget_control, self.id, /destroy
	ptr_free, Self.Curve, self.Parameters
	loadct, 0
END

FUNCTION PMI__Display__FitPatlakModelClemens::Init, parent, CursorPos, xsize=xsize, ysize=ysize

	if n_elements(CursorPos) ne 0 then self.CursorPos = CursorPos

	loadct, 12

	PMI__Info, tlb(parent), Stdy=Stdy

	self.id = widget_base(parent,/column,map=0,event_func='PMI__Display__Event__FitPatlakModelClemens')
	Controls = widget_base(self.id,/row,ysize=40,/base_align_center,space=5)
	self.DrawId	= widget_draw(self.id,/retain)

		v = ['ROI','AIF']
		for i=0,1 do begin
			Base = widget_base(Controls,/row,/frame,/base_align_center)
			id = widget_button(Base, xsize=25, ysize=19, value=v[i], uname=v[i]+'bttn')
  			id = widget_droplist(Base,/dynamic_resize, value=Stdy->Names(1), uname=v[i])
  		endfor

		v = ['Export','Export As','Done']
		Base = widget_base(Controls,/row,/frame,/base_align_center)
		id = widget_button(Base, xsize=60, ysize=22, value='Model Fit', uname='FITbttn')
		for i=0,2 do id = widget_button(Base, xsize=60, ysize=22, value=v[i], uname=v[i])

	self -> Set, xsize=xsize, ysize=ysize, OnDisplay='FIT'
	widget_control, self.id, set_uvalue = self, /map
	return, 1
END

PRO PMI__Display__FitPatlakModelClemens__Define

	Struct = {PMI__Display__FitPatlakModelClemens 	$
	,	id: 0L 	$
	,	DrawId: 0L $
	,	CursorPos:lonarr(4)	$
	,	Curve:ptrarr(3) $ ;RoiCurve, AifCurve, Fit
	,	Parameters: ptr_new() $
	,	Series: obj_new() $
	}
END



pro PMI__Button__Event__FitPatlakModelClemens, ev

	PMI__Info, ev.top, Stdy=Stdy

    Series = Stdy->Names(0,ns,DefDim=3,ind=ind,sel=sel)
    Regions = Stdy->Names(1,nr)

	v = PMI__Form(ev.top, Title='Perfusion analysis setup', [$
		ptr_new({Type:'DROPLIST',Tag:'series', Label:'Dynamic series', Value:Series, Select:sel}), $
		ptr_new({Type:'DROPLIST',Tag:'aif'	 , Label:'Arterial Region', Value:Regions, Select:stdy->sel(1)}), $
		ptr_new({Type:'DROPLIST',Tag:'roi'	 , Label:'Tissue Region', Value:Regions, Select:stdy->sel(1)})])
	IF v.cancel THEN return

	PMI__Control, ev.top, Viewer = 'PMI__Display__FitPatlakModelClemens', Display=Display

	Display -> Set, /Refresh, $
		Series = Stdy->Obj(0,ind[v.series]), $
		set_droplist_select = [v.roi,v.aif]
end

pro PMI__Button__Control__FitPatlakModelClemens, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 1)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__FitPatlakModelClemens, parent,value=value, separator=separator

	if n_elements(value) eq 0 then value = 'Fit Patlak model (Albumin)'

	return, widget_button(parent, $
		value = value,	$
		event_pro = 'PMI__Button__Event__FitPatlakModelClemens', $
		pro_set_value = 'PMI__Button__Control__FitPatlakModelClemens', $
	 	separator = separator )
end
