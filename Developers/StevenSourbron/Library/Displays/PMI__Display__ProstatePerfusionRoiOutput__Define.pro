FUNCTION PMI__Display__ProstatePerfusionRoiOutput::Event, ev

	CASE widget_info(ev.id,/uname) of

		'Save':begin
			PMI__Info, ev.top, Stdy=Stdy
			Path = Stdy->Datapath() + 'Prostate Models'
			file_mkdir, Path
			FileName = Path + '\' + self.Roiname
			Write_tiff, FileName +'.tif', reverse(tvrd(/true),3)
			PMI__write_csv, FileName +'.csv', *self.pars
			end

		'Save As':begin
			PMI__Info, ev.top, State=State
			if not State->get_file(file, file=cleanstr(self.Roiname), title='Save results as..', filter='.tif') then return,0B
			Write_tiff, file, reverse(tvrd(/true),3)
			PMI__write_csv, strmid(file,0,strlen(file)-4) + '.csv', *self.pars
			end

		'Close':begin
			PMI__Control, ev.top, Viewer = 'PMI__DISPLAY__2DVIEW'
			PMI__Control, ev.top, /refresh
			end

	ENDCASE

	return, 0B
END

FUNCTION PMI__Display__Event__ProstatePerfusionRoiOutput, ev

	widget_control, ev.handler, get_uvalue=self
	return, Self -> Event(ev)
END






PRO PMI__Display__ProstatePerfusionRoiOutput::SET, $
	PMI__REFRESH=pmi__refresh, PMI__RESIZE=pmi_resize, $
	Refresh=Refresh, $
 	xsize=xsize, ysize=ysize, $
	Fit = Fit, $
	RoiName = RoiName, $
	Parameters = Pars

	if keyword_set(pmi_resize) then begin
		gpmi = widget_info(/geometry, tlb(self.id))
		self -> Set, xsize=gpmi.xsize, ysize=gpmi.ysize, /REFRESH
	endif
	if n_elements(xsize) ne 0 $
	or n_elements(ysize) ne 0 then widget_control, self.DrawId, xsize=xsize, ysize=ysize-40

	if n_elements(Fit) 			ne 0 then self.Fit = ptr_new(Fit)
	if n_elements(RoiName) 		ne 0 then self.RoiName = RoiName
	if n_elements(Pars) 	ne 0 then self.Pars = ptr_new(Pars)


	if keyword_set(Refresh) then begin

		widget_control, self.DrawId, get_value = win
		wset, win & loadct, 0 & erase
		if not ptr_valid(self.Fit) then return

		MinC = min((*self.Fit)[1:*,*], Max=MaxC)

 		plot, /nodata, position=[0.1,0.1,0.6,0.90]  $
 		,	Title = self.RoiName $
		, 	[0,max((*self.Fit)[0,*])], [MinC,MaxC] $
		, 	/xstyle, /ystyle $
		, 	background=255, color=0 $
		, 	xtitle = 'Time (sec)', ytitle='Relative Signal Enhancement' $
		, 	charsize=2.0, charthick=2.0, xthick=2.0, ythick=2.0

		loadct, 12

		clr=[14,0,6,9,2,12]
		for i=1,6 do oplot, (*self.Fit)[0,*], (*self.Fit)[i,*], color=clr[i-1]*16, linestyle=0, thick=2

		py=0.9 & dy=0.0220 & x0=0.625 & charsize=1.5 & charthick=1.5 & p = *self.Pars

		xyouts,x0,py,p[2,0],color=0*16,/normal,charsize=charsize,charthick=charthick
		i = [1,7,8]
		for k=0L,n_elements(i)-1 do begin
			py=py-dy
			xyouts,x0,py,p[0,i[k]]+' = '+p[2,i[k]]+' '+p[1,i[k]],color=0,/normal,charsize=charsize,charthick=charthick
		endfor
		py=py-2*dy
		xyouts,x0,py,p[3,0],color=6*16,/normal,charsize=charsize,charthick=charthick
		i = [1,7,8,10,11]
		for k=0L,n_elements(i)-1 do begin
			py=py-dy
			xyouts,x0,py,p[0,i[k]]+' = '+p[3,i[k]]+' '+p[1,i[k]],color=0,/normal,charsize=charsize,charthick=charthick
		endfor
		py=py-2*dy
		xyouts,x0,py,p[4,0],color=9*16,/normal,charsize=charsize,charthick=charthick
		i = [1,2,3,4,10,11]
		for k=0L,n_elements(i)-1 do begin
			py=py-dy
			xyouts,x0,py,p[0,i[k]]+' = '+p[4,i[k]]+' '+p[1,i[k]],color=0,/normal,charsize=charsize,charthick=charthick
		endfor
		py=py-2*dy
		xyouts,x0,py,p[5,0],color=2*16,/normal,charsize=charsize,charthick=charthick
		i = [2,4,5,6,10,11]
		for k=0L,n_elements(i)-1 do begin
			py=py-dy
			xyouts,x0,py,p[0,i[k]]+' = '+p[5,i[k]]+' '+p[1,i[k]],color=0,/normal,charsize=charsize,charthick=charthick
		endfor
		py=py-2*dy
		xyouts,x0,py,p[6,0],color=12*16,/normal,charsize=charsize,charthick=charthick
		i = [1,2,3,4,5,6,10,11]
		for k=0L,n_elements(i)-1 do begin
			py=py-dy
			xyouts,x0,py,p[0,i[k]]+' = '+p[6,i[k]]+' '+p[1,i[k]],color=0,/normal,charsize=charsize,charthick=charthick
		endfor

		loadct, 0

	endif

END



PRO PMI__Display__ProstatePerfusionRoiOutput::GET, CursorPos = CursorPos

	if arg_present(CursorPos) then CursorPos=self.CursorPos

END


PRO PMI__Display__ProstatePerfusionRoiOutput::Cleanup
	widget_control, self.id, /destroy
	ptr_free, self.Pars, self.Fit
END

FUNCTION PMI__Display__ProstatePerfusionRoiOutput::Init, parent, CursorPos, xsize=xsize, ysize=ysize

	self.CursorPos = CursorPos

	self.id = widget_base(parent,/column,map=0,event_func='PMI__Display__Event__ProstatePerfusionRoiOutput')

	Controls = widget_base(self.id,/row,ysize=35)
	self.DrawId	= widget_draw(self.id,/retain)

		Buttons = widget_base(Controls,/row,/frame)
		ybttn=20
		id = widget_button(Buttons, ysize=ybttn, value='Save'	,uname='Save')
		id = widget_button(Buttons, ysize=ybttn, value='Save As',uname='Save As')
		id = widget_button(Buttons, ysize=ybttn, value='Close'	,uname='Close')

	self -> Set, xsize=xsize, ysize=ysize

	widget_control, self.id, set_uvalue = self, /map
	return, 1
END

PRO PMI__Display__ProstatePerfusionRoiOutput__Define

	Struct = {PMI__Display__ProstatePerfusionRoiOutput 	$
	,	id: 0L 	$
	,	DrawId:0L $
	,	CursorPos:lonarr(4)	$
	,	Pars: ptr_new() $
	,	RoiName:'' $
	,	Fit:ptr_new() $
	}
END

