;
PRO PMI__Display__iBEAt_PC_ROI::Fit

	Self->GET, Model=Model, Time=Time, RoiCurve=Curve,Stdy=Stdy
	Self->SET, Message='Fitting...', Sensitive=0
	d = self.series -> d()
    pixel_spacing = self.series -> GETVALUE('0028'x,'0030'x)
    rescale_slope  = self.series -> GETVALUE('0028'x,'1053'x)
    rescale_intercept = self.series -> GETVALUE('0028'x,'1052'x)

    Signal= *Self.Curve[0]


   ; ; rescale slope and intercept: scaling factor for PC-MRI pixels: not required in pmi
   ; velocity_encoding = 60 ; velocity encoding hardcoded: tbc with steven
   ; new_signal_with_scaling_factor = ABS(((Signal*rescale_slope + rescale_intercept)/4096)*velocity_encoding)

    Curve = reform(Signal,[N_ELEMENTS(Curve),1]) ; number of cardiac phases = 20


	CASE Model OF


		'PC-RBF':begin

            Region='ROI'
	    	Self->GET, Stdy=Stdy, Region=Region

            v = PMI__RoiValues(Stdy->DataPath(), Self.series, Region, cnt=npix)
            ROIstatTotalPixel = npix

            PC_velocity  = MEAN(Curve)

            RBF = (PC_velocity*ROIstatTotalPixel*pixel_spacing[0]*pixel_spacing[1]*0.001/0.0166667)/2 ; (mm/sec)*mm2=mm3/sec->mm3/sec*0.001->ml/sec*1/0.016667->ml/min
            ; mm/sec*mm2->mm3/sec->ml/sec(*0.001/0.016667): velocity*totalpixels*pixelarea*ml_conversion*per_min_conversion


            Par = FLTARR(d[0],d[1],3) ; original par dim
            P = reform(Par[0,0,*],[n_elements(Par[0,0,*]),1]) ; reform for pixelwise fitting instead of whole image: 7 col, 1 row
            P[0,0] = RBF
            P[1,0] = MAX(Curve)
            P[2,0] = MEAN(Curve)

            Fit = Curve

			Parameters = $
			    [{Name:'PC-RBF'			,Units:'ml/min'		,Value: P[0,0]	,Nr: 0} $
				, {Name:'peak velocity'			,Units:'mm/sec'		,Value:P[1,0]	,Nr: 1}$
				, {Name:'mean velocity'			,Units:'mm/sec'		,Value:P[2,0]	,Nr: 2}]
            end


	endcase

	self.Curve[1] = ptr_new(Fit)
	self.Parameters = ptr_new(Parameters)
	Self->SET, /Sensitive
END




PRO PMI__Display__iBEAt_PC_ROI::Plot

	Self->GET, OnDisplay=OnDisplay

	top=0.9 & dy=0.04 & x0=0.525 & charsize=1.0 & charthick=1.0

	CASE OnDisplay OF

		'ROI':begin
			Self -> GET, Time=Time, RoiCurve=Y, RoiName=RoiName, Units=Units
			Self -> SET, /Erase

 			plot, /nodata, position=[0.1,0.2,0.5,0.9]  $
			, 	[0,0.35], [min(Y),max(Y)] $
			, 	/xstyle, /ystyle $
			, 	background=255, color=0 $
			, 	xtitle = 'Time (sec)', ytitle=Units $
			, 	charsize=1.5, charthick=2.0, xthick=2.0, ythick=2.0

			oplot, time, Y, color=6*16, psym=4, thick=2
			xyouts, x0, top-0*dy, 'Region Of Interest: ' + RoiName, color=6*16, /normal, charsize=1.5, charthick=1.5
			end


		'FIT':BEGIN
			Self -> GET, RoiCurve=RoiCurve, Time=Time, Fit=Fit, Model=Model, RoiName=RoiName, Units=Units
			Self -> SET, /Erase
 			plot, /nodata, position=[0.1,0.2,0.5,0.9]  $
			, 	[0,0.35], [min([min(RoiCurve),min(Fit)]),max([max(RoiCurve),max(Fit)])] $
			, 	/xstyle, /ystyle $
			, 	background=255, color=0 $
			, 	xtitle = 'Time (sec)', ytitle=Units $
			, 	charsize=1.5, charthick=2.0, xthick=2.0, ythick=2.0

			oplot, time, RoiCurve, color=6*16, linestyle=0, thick=2


			xyouts, x0, top-0*dy, 'Region Of Interest: ' + RoiName		, color=6*16, /normal, charsize=1.5, charthick=1.5
			xyouts, x0, top-3*dy, 'PC Tissue Model: ' + Model				, color=12*16, /normal, charsize=1.5, charthick=1.5

			P = *self.Parameters

			for i=0L,n_elements(P)-1 do xyouts $
				, x0, top-dy*(5+P[i].Nr) $
				, P[i].Name + ' = ' + PMI__Round(P[i].Value,3,/string) + ' ' + P[i].Units $
				, color=0, /normal, charsize=charsize, charthick=charthick
			END
	ENDCASE

END




FUNCTION PMI__Display__iBEAt_PC_ROI::Event, ev ; add event for roi name

	Uname = widget_info(ev.id,/uname)

	if Uname Eq 'FITbttn' then begin
		widget_control, ev.id, sensitive=0
		widget_control, widget_info(self.id,find_by_uname='ROIbttn'), sensitive=1
		list = widget_info(self.id,find_by_uname='FIT')
		widget_control, list, sensitive=1
		self->plot
		return, 0B
	endif

	if Uname Eq 'ROIbttn' then begin
		widget_control, ev.id, sensitive=0
		widget_control, widget_info(self.id,find_by_uname='FITbttn'), sensitive=1
		widget_control, widget_info(self.id,find_by_uname='FIT'), sensitive=0
		self->plot
		return, 0B
	endif

   	i = where(Uname Eq ['ROI'], cnt) ; added for different roi selection from dropdown menu list
	If cnt eq 1 then begin
		ptr_free, Self.Curve[i], Self.Curve[1], self.parameters
		self->plot
		return, 0B
	endif


	if Uname Eq 'FIT' then begin
		widget_control, sensitive=widget_info(ev.id,/droplist_select) GT 1
		ptr_free, Self.Curve[1], self.parameters
		self -> Plot
		return, 0B
	endif


	i = where(Uname Eq ['Export','Export As'], cnt)
	if cnt eq 1 then begin
		Self->GET, Time=Time, RoiCurve=RoiCurve, Fit=Fit, Model=Model, Roiname=Roiname
		if Uname eq 'Export' then begin
			PMI__Info, ev.top, Stdy=Stdy
			Path = Stdy->Datapath() + 'PC Kidney Models (ROI)'
			file_mkdir, Path
			File = Path + '\' + Roiname + '__SI_' + Model
		endif else begin
			PMI__Info, ev.top, State=State
			if not State -> Get_file(file, file=cleanstr(Roiname + '__' + Model), title='Save as..', filter='.tif') then return,0B
			File = strmid(file,0,strlen(file)-4)
		endelse
		Write_tiff, File + '.tif', reverse(tvrd(/true),3)

		Export = strarr(3,1+n_elements(time))
		Export[*,0] = ['Time (s)','Curve', 'Fit']
		Export[0,1:*] = strcompress(Time,/remove_all)
		Export[1,1:*] = strcompress(RoiCurve,/remove_all)
		Export[2,1:*] = strcompress(Fit,/remove_all)
		PMI__Write_csv, File +'__Signals.csv', Export

		Export = strarr(3,1+n_elements(*self.Parameters))
		Export[*,0] = ['Parameter','Units', 'Value']
		Export[0,1:*] = (*self.Parameters).Name
		Export[1,1:*] = (*self.Parameters).Units
		Export[2,1:*] = strcompress((*self.Parameters).Value,/remove_all)
		PMI__Write_csv, File + '__Parameters.csv', Export

		return, 0B
	endif

	Menu = widget_info(widget_info(ev.top,/child),/all_children)
	for i=0L,n_elements(Menu)-1 do widget_control, Menu[i], /sensitive
	PMI__Control, ev.top, Viewer = 'PMI__DISPLAY__2DVIEW'
	PMI__Control, ev.top, /refresh
	return, 0B
END

FUNCTION PMI__Display__Event__iBEAt_PC_ROI, ev

	widget_control, ev.handler, get_uvalue=self
	return, Self -> Event(ev)
END



FUNCTION PMI__Display__iBEAt_PC_ROI::Conc, Region ;

	case Region of
		'ROI':Signal=*Self.Curve[0] ; signal
	endcase
	Self -> GET, SignalModel=SignalModel
	If SignalModel eq 0 then return, Signal


END



FUNCTION PMI__Display__iBEAt_PC_ROI::GetCurve, Region

	Self -> GET, Time=Time, Stdy=Stdy, Region=Region
	Self -> SET, Message = 'Loading ' + Region->Name() + ' curve', Sensitive=0
	Signal = PMI__RoiCurve(Stdy->DataPath(), Self.Series, Region, cnt=cnt)
	Self -> SET, /Sensitive
	if cnt eq 0 then return, Time*0
	return, Signal
END

FUNCTION PMI__Display__iBEAt_PC_ROI::GetName, Region

	self->GET, Region=Region
	return, Region -> Name()
END


PRO PMI__Display__iBEAt_PC_ROI::GET, $
 	CursorPos = CursorPos, $
	Model=Model, $
	Time=Time, Fit=Fit, Units=Units, SignalModel=SignalModel, $
 	RoiCurve=RoiCurve, $
	RoiName=RoiName, $
 	OnDisplay=OnDisplay, $
	RoiVolume=RoiVolume, $
	Region = Region, Stdy=Stdy

	if arg_present(CursorPos) then CursorPos=self.CursorPos

	if arg_present(Model) then begin
		list = widget_info(self.id,find_by_uname='FIT')
		widget_control, list, Get_Value=Models
		Model = Models[widget_info(list,/droplist_select)]
	endif

	if arg_present(Time) then begin
		t = Self.Series -> c(1)
		Time = t-t[0]
	endif

	if arg_present(RoiName) then RoiName=Self->GetName('ROI')

	if arg_present(RoiCurve) then begin
		if not ptr_valid(Self.Curve[0]) then Self.Curve[0] = ptr_new(Self->GetCurve('ROI'))
		RoiCurve = self->Conc('ROI')
	endif

	if arg_present(FIT) then begin
		if not ptr_valid(Self.Curve[1]) then Self->Fit
		FIT = *Self.Curve[1]
		endif

	if arg_present(units) then begin
		Self -> GET, SignalModel=tmp
		case tmp of
		0:units = 'PC velocity (mm/sec)'
		endcase
	endif

	if arg_present(SignalModel) then begin
		id = widget_info(self.id,find_by_uname='SIG')
		SignalModel = widget_info(id,/droplist_select)
	endif

	if arg_present(OnDisplay) then begin
		OnDisplay = ''
		List = ['ROI','FIT']
		for i=0L,1 do $
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

PRO PMI__Display__iBEAt_PC_ROI::SET, $
	PMI__REFRESH=pmi__refresh, PMI__RESIZE=pmi_resize, $
	Refresh=Refresh, Erase=Erase, $
	Message=Message, Sensitive=Sensitive, $
	xsize=xsize, ysize=ysize, $
	Set_droplist_select = Set_droplist_select, $
	Series=Series, $
	OnDisplay=OnDisplay

	if keyword_set(pmi_resize) then begin
		gpmi = widget_info(/geometry, tlb(self.id))
		self -> Set, xsize=gpmi.xsize, ysize=gpmi.ysize
		self->plot
	endif

	IF n_elements(Series) NE 0 THEN BEGIN
		Self.Series = Series
	ENDIF
	if n_elements(Message) 	ne 0 then begin
		Self->Set, /Erase
		xyouts, 0.1,0.9, Message, color=0,/normal,charsize=1.5,charthick=2.0
	endif
	if n_elements(Sensitive) ne 0 then widget_control, self.id, sensitive=sensitive
	if n_elements(Set_droplist_select) ne 0 then begin
		List = ['ROI']
		for i=0,0 do widget_control, $
			widget_info(self.id,find_by_uname=List[i]), $
			Set_droplist_select=Set_droplist_select[i]
	endif
	if n_elements(xsize) ne 0 $
	or n_elements(ysize) ne 0 then begin
		widget_control, self.DrawId, xsize=xsize, ysize=ysize-40
		xs = floor((xsize - 750)/2E)
		if xs lt 50 then xs = 50
		List = ['ROI']
		for i=0,0 do widget_control, widget_info(self.id,find_by_uname=List[i]), xsize=xs
	endif

	if keyword_set(Erase) then begin
		widget_control, self.DrawId, get_value = win
		wset, win & erase, 255
	endif

	if n_elements(OnDisplay) ne 0 then begin

		List = ['ROI','FIT']
		for i=0,1 do $
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




PRO PMI__Display__iBEAt_PC_ROI::Cleanup
	widget_control, self.id, /destroy
	ptr_free, Self.Curve, self.Parameters
	loadct, 0
END


FUNCTION PMI__Display__iBEAt_PC_ROI::Init, parent, CursorPos, xsize=xsize, ysize=ysize ; DEFINE DISPLAY: CHANGE ROI

	if n_elements(CursorPos) ne 0 then self.CursorPos = CursorPos

	loadct, 12

	PMI__Info, tlb(parent), Stdy=Stdy

	self.id = widget_base(parent,/column,map=0,event_func='PMI__Display__Event__iBEAt_PC_ROI')
	Controls = widget_base(self.id,/row,ysize=40,/base_align_center,space=5)
	self.DrawId	= widget_draw(self.id,/retain)

		v = ['ROI']
		for i=0,0 do begin
			Base = widget_base(Controls,/row,/frame,/base_align_center) ; frame with no of buttons
			id = widget_button(Base, xsize=25, ysize=19, value=v[i], uname=v[i]+'bttn'); button; username: ROIbutton
  			id = widget_droplist(Base,/dynamic_resize, value=Stdy->Names(1), uname=v[i]);username: ROI
  		endfor

		Base = widget_base(Controls,/row,/frame,/base_align_center)
			id = widget_droplist(Base,/dynamic_resize, uname='SIG',value = ['PC Signal (a.u.)']) ; name of the signal curve units
  			widget_control, id, set_droplist_select = 4

		Base = widget_base(Controls,/row,/frame,/base_align_center)
			id = widget_button(Base, xsize=25, ysize=19, value='FIT', uname='FITbttn')
			id = widget_droplist(Base,/dynamic_resize, uname='FIT',value = ['PC-RBF']) ; name of the fitting
  			widget_control, id, set_droplist_select = 5

		v = ['Export','Export As','Close']
		Base = widget_base(Controls,/row,/frame,/base_align_center)
		for i=0,2 do id = widget_button(Base, xsize=50, ysize=22, value=v[i], uname=v[i])

	self -> Set, xsize=xsize, ysize=ysize, OnDisplay='FIT'
	widget_control, self.id, set_uvalue = self, /map
	return, 1
END

PRO PMI__Display__iBEAt_PC_ROI__Define


	Struct = {PMI__Display__iBEAt_PC_ROI 	$
	,	id: 0L 	$
	,	DrawId: 0L $
	,	CursorPos:lonarr(4)	$
	,	Curve:ptrarr(2) $ ;RoiCurve, Fit
	,	Parameters: ptr_new() $
	,	Series: obj_new() $
	}
END


pro PMI__Button__Event__iBEAt_PC_ROI, ev

	PMI__Info, ev.top, Stdy=Stdy

    Series = Stdy->Names(0,ns,DefDim=3,ind=ind,sel=sel)
    Regions = Stdy->Names(1,nr)

	v = PMI__Form(ev.top, Title='PC RBF analysis setup', [$
		ptr_new({Type:'DROPLIST',Tag:'series', Label:'PC series', Value:Series, Select:sel}), $
		ptr_new({Type:'DROPLIST',Tag:'roi'	 , Label:'Tissue Region', Value:Regions, Select:stdy->sel(1)})])



	IF v.cancel THEN return

	PMI__Control, ev.top, Viewer = 'PMI__Display__iBEAt_PC_ROI', Display=Display

	Display -> Set, /Refresh, $
		Series = Stdy->Obj(0,ind[v.series]), $
		set_droplist_select = [v.roi]
end

pro PMI__Button__Control__iBEAt_PC_ROI, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3) ; ns : no of series with time dim
		Regions = Stdy->Names(1,nr) ;nr number of regions
		sensitive = (ns gt 0) and (nr gt 0)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__iBEAt_PC_ROI, parent,value=value, separator=separator

	PMI__Display__iBEAt_PC_ROI__Define

	if n_elements(value) eq 0 then value = 'Renal PC MAP based model (ROI)'

	return, widget_button(parent, $
		value = value,	$
		event_pro = 'PMI__Button__Event__iBEAt_PC_ROI',	$
		pro_set_value = 'PMI__Button__Control__iBEAt_PC_ROI', $
	 	separator = separator )
end