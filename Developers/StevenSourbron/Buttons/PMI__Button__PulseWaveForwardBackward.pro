FUNCTION PMI__Button__Display__PulseWaveForwardBackward::Event, ev

	Uname = widget_info(ev.id,/uname)

	i = where(Uname Eq ['Series','Region','Parameter'], cnt)
	If cnt eq 1 then begin
		Self->SET, /Load, /Refresh
		return, 0B
	endif

	i = where(Uname Eq ['Export','Export As'], cnt)
	if cnt eq 1 then begin
		if Uname eq 'Export' then begin
			PMI__Info, ev.top, Stdy=Stdy
			Path = Stdy->Datapath() + 'PWV output'
			file_mkdir, Path
			File = Path + '\' + 'PWV_Curve'
		endif else begin
			PMI__Info, ev.top, State=State
			if not State -> Get_file(file, file='PWV_Curve', title='Save as..', filter='.tif') then return,0B
			File = strmid(file,0,strlen(file)-4)
		endelse
		Write_tiff, File + '.tif', reverse(tvrd(/true),3)
		PMI__WritePlot, File + '__All.txt', *self.Curve[0], *self.Curve[1]
		PMI__WritePlot, File + '__Pos.txt', *self.Curve[0], *self.Curve[2]
		PMI__WritePlot, File + '__Neg.txt', *self.Curve[0], *self.Curve[3]
		return, 0B
	endif

	PMI__Control, ev.top, Viewer = 'PMI__DISPLAY__2DVIEW'
	PMI__Control, ev.top, /refresh
	return, 0B
END


PRO PMI__Button__Display__PulseWaveForwardBackward::Refresh

	Menu = widget_info(widget_info(tlb(self.id),/child),/all_children)
	for i=0L,n_elements(Menu)-1 do widget_control, Menu[i], sensitive=0


	Time = *self.Curve[0]
	All = *self.Curve[1]
	Pos = *self.Curve[2]
	Neg = *self.Curve[3]

	Par = ['Flow (l/min)','Cross Section (cm^2)', 'Mean Velocity (cm/sec)']
	Par = Par[widget_info(widget_info(self.id,find_by_uname='Parameter'),/droplist_select)]

	top=0.9 & dy=0.04 & x0=0.525 & charsize=1.0 & charthick=1.0

 	plot, /nodata, position=[0.1,0.2,0.5,0.9]  $
	, 	[0,max(time)], [min([min(All),min(Pos),min(Neg)]),max([max(All),max(Pos),max(Neg)])] $
	, 	/xstyle, /ystyle, background=255, color=0 $
	, 	xtitle = 'Time (msec)', ytitle=Par $
	, 	charsize=1.5, charthick=2.0, xthick=2.0, ythick=2.0
	oplot, time, All, color=0*16, linestyle=0, thick=2
	oplot, time, Pos, color=12*16, linestyle=0, thick=2
	oplot, time, Neg, color=6*16, linestyle=0, thick=2

	xyouts, x0, top-1*dy, 'Red: Forward flow' , color=12*16, /normal, charsize=1.5, charthick=1.5
	xyouts, x0, top-2*dy, 'Blue: Backward flow' , color=6*16, /normal, charsize=1.5, charthick=1.5
	xyouts, x0, top-3*dy, 'Black: Total flow'		, color=0*16, /normal, charsize=1.5, charthick=1.5

	P = *self.Parameters
	for i=0L,n_elements(P)-1 do xyouts $
		, x0, top-dy*(5+P[i].Nr) $
		, P[i].Name + ' = ' + PMI__Round(P[i].Value,6,/string) + ' ' + P[i].Units $
		, color=0, /normal, charsize=1.5, charthick=1.5

END

PRO PMI__Button__Display__PulseWaveForwardBackward::Load

	PMI__Info, tlb(self.id), Stdy=Stdy
	Series = Stdy->Names(0,DefDim=3,ind=ind)
	Series = Stdy->Obj(0,ind[widget_info(widget_info(self.id,find_by_uname='Series'),/droplist_select)])
		;Series consists of three images: magnitude, magnitude*abs(phase), phase (units: velocity)
		;or two images: magnitude, phase
	nt = Series->Get('2001'x,'1017'x) ;number of phases
	nt = nt[0]
	nser = Series->d(3)/nt ;number of series (2 or 3)
	Time = Series->t()
	Time = Time[0:nt-1]-Time[0]

	;Express times in msec
	HeartRate = Series->GetValue('0018'x,'1088'x) ;beats per min
	If HeartRate eq 0 then begin
		in = PMI__Form(ev.top, Title='Please enter the heart rate', [$
			ptr_new({Type:'VALUE', Tag:'rate' , Label:'Heart rate (beats per min)', Value:70E}) ])
	;	Series->SetValue, '0018'x,'1088'x, in.rate
		HeartRate = in.rate
	endif
	HeartRate = HeartRate/60E ;beats per sec
	Time = 1000*Time/(HeartRate*nt)  ;time in units of msec

		;Extrude ROI if only defined at one time point

		;ROIs are drawn on the magnitude image, but should be evaluated on the velocity image
		;=> Shift Series time coordinates to extract ROI curves, then restore
	Series -> t, shift(Series->t(),(nser-1)*nt)
	Region = Stdy->Obj(1,widget_info(widget_info(self.id,find_by_uname='Region'),/droplist_select))
	Parameter = widget_info(widget_info(self.id,find_by_uname='Parameter'),/droplist_select)
	All = fltarr(nt) & Pos=All & Neg=All
	PixelSize = Series->Get('0028'x,'0030'x)/10E ;cm
	If n_elements(Pixelsize) eq 2 then PixelArea=Pixelsize[0]*Pixelsize[1] else PixelArea=Pixelsize[0]^2
	MeanVelocity = 0E
	for k=0L,nt-1 do begin
		velocity = PMI__RoiSliceValues(Stdy->DataPath(), Series, Region, Region->z(0), Region->t(k), cnt=cnt) ;cm/sec
		if cnt ne 0 then begin
			MeanVelocity = MeanVelocity + total(velocity)/cnt
			pos_ind = where(velocity ge 0, npos_ind)
			neg_ind = where(velocity lt 0, nneg_ind)
			case Parameter of
			0:begin
				All[k]=total(velocity)*PixelArea*60E/1000E ;l/min
				if npos_ind gt 0 then Pos[k]=total(velocity[pos_ind])*PixelArea*60E/1000E
				if nneg_ind gt 0 then Neg[k]=total(-velocity[neg_ind])*PixelArea*60E/1000E
				end
			1:begin
				All[k]=cnt*PixelArea ;cm^2
				Pos[k]=npos_ind*PixelArea
				Neg[k]=nneg_ind*PixelArea
				end
			2:begin
				All[k]=total(velocity)/cnt ;cm/sec
				if npos_ind gt 0 then Pos[k]=total(velocity[pos_ind])/npos_ind
				if nneg_ind gt 0 then Neg[k]=total(-velocity[neg_ind])/nneg_ind
				end
;			3:begin
;				All[k]=max(abs(velocity)) ;cm/sec
;				if npos_ind gt 0 then Pos[k]=max(velocity[pos_ind])
;				if nneg_ind gt 0 then Neg[k]=max(-velocity[neg_ind])
;				end
;			4:begin
;				All[k]=min(abs(velocity)) ;cm/sec
;				if npos_ind gt 0 then Pos[k]=min(velocity[pos_ind])
;				if nneg_ind gt 0 then Neg[k]=min(-velocity[neg_ind])
;				end
			endcase
		endif
	endfor
	If MeanVelocity lt 0 then begin
		if (Parameter eq 0) or (Parameter Eq 2) then All=-All
		tmp = pos
		pos = neg
		neg = tmp
	endif
	Series -> t, shift(Series->t(),-(nser-1)*nt)

	case Parameter of
		0:Indices = $
			[{Name:'Stroke volume'			,Units:'ml'	,Value:1000D*int_tabulated(time/1000E/60E,All)	,Nr: 0} $
			,{Name:'Forward stroke volume'	,Units:'ml'	,Value:1000D*int_tabulated(time/1000E/60E,Pos)	,Nr: 1} $
			,{Name:'Backward stroke volume'	,Units:'ml'	,Value:1000D*int_tabulated(time/1000E/60E,Neg)	,Nr: 2} ]
		1:Indices = $
			[{Name:'Average Cross Section'			,Units:'cm^2'	,Value:1D*total(All)/nt	,Nr: 0} $
			,{Name:'Average Forward Cross Section'	,Units:'cm^2'	,Value:1D*total(Pos)/nt	,Nr: 1} $
			,{Name:'Average Backward Cross Section'	,Units:'cm^2'	,Value:1D*total(Neg)/nt	,Nr: 2} ]
		2:Indices = $
			[{Name:'Average Velocity'			,Units:'cm/sec'	,Value:1D*total(All)/nt	,Nr: 0} $
			,{Name:'Average Forward Velocity'	,Units:'cm/sec'	,Value:1D*total(Pos)/nt	,Nr: 1} $
			,{Name:'Average Backward Velocity'	,Units:'cm/sec'	,Value:1D*total(Neg)/nt	,Nr: 2} ]
	endcase

	ptr_free, Self.Curve, self.Parameters
	Self.Curve = [ptr_new(Time),ptr_new(All),ptr_new(Pos),ptr_new(Neg)]
	self.Parameters = ptr_new(Indices)
END


PRO PMI__Button__Display__PulseWaveForwardBackward::GET, $
	CursorPos = CursorPos

	if arg_present(CursorPos) then CursorPos=self.CursorPos
END


PRO PMI__Button__Display__PulseWaveForwardBackward::SET, $
	PMI__REFRESH=pmi__refresh, PMI__RESIZE=pmi_resize, $
	Refresh=Refresh, Load=Load, $
	xsize=xsize, ysize=ysize

	if keyword_set(pmi_resize) then begin
		gpmi = widget_info(/geometry, tlb(self.id))
		self -> Set, xsize=gpmi.xsize, ysize=gpmi.ysize, /REFRESH
	endif

	if n_elements(xsize) ne 0 $
	or n_elements(ysize) ne 0 then begin
		widget_control, self.DrawId, xsize=xsize, ysize=ysize-40
		xs = floor((xsize - 365)/3E)
		if xs lt 50 then xs = 50
		List = ['Series','Region','Parameter']
		for i=0,2 do widget_control, widget_info(self.id,find_by_uname=List[i]), xsize=xs
	endif

	if keyword_set(Load) then Self->Load
	if keyword_set(Refresh) then Self -> Refresh
END


FUNCTION PMI__Button__Display__PulseWaveForwardBackward__Event, ev
	widget_control, ev.handler, get_uvalue=self
	return, Self -> Event(ev)
END


PRO PMI__Button__Display__PulseWaveForwardBackward::Cleanup
	Menu = widget_info(widget_info(self.id,/child),/all_children)
	for i=0L,n_elements(Menu)-1 do widget_control, Menu[i], /sensitive
	widget_control, self.id, /destroy
	ptr_free, Self.Curve, self.Parameters
	loadct, 0
END


FUNCTION PMI__Button__Display__PulseWaveForwardBackward::Init, parent, CursorPos, xsize=xsize, ysize=ysize

	if n_elements(CursorPos) ne 0 then self.CursorPos = CursorPos

	loadct, 12

	PMI__Info, tlb(parent), Stdy=Stdy

	self.id = widget_base(parent,/column,map=0,event_func='PMI__Button__Display__PulseWaveForwardBackward__Event')
	Controls = widget_base(self.id,/row,ysize=40,/base_align_center,space=5)
	self.DrawId	= widget_draw(self.id,/retain)

		Series = Stdy->Names(0,DefDim=3,sel=sel)
		Base = widget_base(Controls,/row,/frame,/base_align_center)
		id = widget_label(Base, xsize=40, ysize=15, value='Series')
  		id = widget_droplist(Base,/dynamic_resize, value=Series, uname='Series')
  		widget_control, id, set_droplist_select=sel

		Base = widget_base(Controls,/row,/frame,/base_align_center)
		id = widget_label(Base, xsize=40, ysize=15, value='Region')
  		id = widget_droplist(Base,/dynamic_resize, value=Stdy->Names(1), uname='Region')
  		widget_control, id, set_droplist_select=Stdy->sel(1)

		Base = widget_base(Controls,/row,/frame,/base_align_center)
		id = widget_label(Base, xsize=50, ysize=15, value='Parameter')
  		id = widget_droplist(Base,/dynamic_resize, value=['Flow (l/min)','Cross Section (cm^2)', 'Mean Velocity (cm/sec)'], uname='Parameter')
		widget_control, id, set_droplist_select=0

		v = ['Export','Export As','Close']
		Base = widget_base(Controls,/row,/frame,/base_align_center)
		for i=0,2 do id = widget_button(Base, xsize=50, ysize=22, value=v[i], uname=v[i])

	self -> Set, xsize=xsize, ysize=ysize, /Load
	widget_control, self.id, set_uvalue = self, /map
	self -> Set, /Refresh

	return, 1
END

PRO PMI__Button__Display__PulseWaveForwardBackward__Define

	Struct = {PMI__Button__Display__PulseWaveForwardBackward 	$
	,	id: 0L 	$
	,	DrawId: 0L 	$
	,	CursorPos:lonarr(4)	$
	,	Curve:ptrarr(4) $ ;Time, Total, Positive, Negative
	,	Parameters: ptr_new() $
	}
END



pro PMI__Button__Event__PulseWaveForwardBackward, ev

	PMI__Control, ev.top, Viewer = 'PMI__Button__Display__PulseWaveForwardBackward', Display=Display
	Display -> Set, /Refresh
end


pro PMI__Button__Control__PulseWaveForwardBackward, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 0)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__PulseWaveForwardBackward, parent,value=value, separator=separator

	PMI__Display__ViewRoiCurve__Define

	if n_elements(value) eq 0 then value = 'Pulse Wave Velocity - Forward/Backward Flow'

	id = widget_button(parent $
	,	value 		= value	$
	,	event_pro 	= 'PMI__Button__Event__PulseWaveForwardBackward'$
	,	pro_set_value 	= 'PMI__Button__Control__PulseWaveForwardBackward' $
	, 	separator 	= separator	)

	return, id

end
