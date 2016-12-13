FUNCTION PMI__Display__RoiFit::Event, ev

	CASE widget_info(ev.id,/uname) of

		'Save':begin
			PMI__Info, ev.top, Stdy=Stdy
			Write_tiff, Stdy->Datapath() + self.title + '.tif', reverse(tvrd(/true),3)
			end

		'Save As':begin
			PMI__Info, ev.top, State=State
			if not State->get_file(file, file=cleanstr(self.title), title='Save plot as..', filter='.tif') then return,0B
			Write_tiff, file, reverse(tvrd(/true),3)
			end

		'Close':begin
			PMI__Control, ev.top, Viewer = 'PMI__DISPLAY__2DVIEW'
			PMI__Control, ev.top, /refresh
			end

	ENDCASE

	return, 0B
END

FUNCTION PMI__Display__Event__RoiFit, ev

	widget_control, ev.handler, get_uvalue=self
	return, Self -> Event(ev)
END


PRO PMI__Display__RoiFit::PLOT, X, Y, F, P, title=title, xtitle=xtitle, ytitle=ytitle, xrange=xrange, yrange=yrange

	if ptr_valid(self.X) then ptr_free, *self.X, *self.Y, *self.F, *self.P
	ptr_free, self.X, self.Y, self.F, self.P
	self.X = ptr_new(ptrarr(1))
	self.Y = ptr_new(ptrarr(1))
	self.F = ptr_new(ptrarr(1))
	self.P = ptr_new(ptrarr(1))
	*self.X[0] = ptr_new(X)
	*self.Y[0] = ptr_new(Y)
	*self.F[0] = ptr_new(F)
	*self.P[0] = ptr_new(P)

	if n_elements(title) ne 0 then self.title = title
	if n_elements(xtitle) ne 0 then self.xtitle = xtitle
	if n_elements(ytitle) ne 0 then self.ytitle = ytitle
	if n_elements(xrange) ne 0 then self.xrange = xrange else self.xrange=[min(X),max(X)]
	if n_elements(yrange) ne 0 then self.yrange = yrange else self.yrange=[min(Y),max(Y)]

	self -> SET, /Refresh
END

PRO PMI__Display__RoiFit::OPLOT, X, Y, F, P

	*self.X = [*self.X,ptr_new(X)]
	*self.Y = [*self.Y,ptr_new(Y)]
	*self.F = [*self.F,ptr_new(F)]
	*self.P = [*self.P,ptr_new(P)]

	self -> SET, /Refresh
END


PRO PMI__Display__RoiFit::SET, $
	PMI__REFRESH=pmi__refresh, PMI__RESIZE=pmi_resize, $
	xsize=xsize, ysize=ysize, $
	Refresh=Refresh


	if keyword_set(pmi_resize) then begin
		gpmi = widget_info(/geometry, tlb(self.id))
		self -> Set, xsize=gpmi.xsize, ysize=gpmi.ysize, /REFRESH
	endif
	if n_elements(xsize) ne 0 $
	or n_elements(ysize) ne 0 then widget_control, self.DrawId, xsize=xsize, ysize=ysize-40

	if keyword_set(Refresh) then begin

		widget_control, self.DrawId, get_value = win
		wset, win & erase

 		plot, /nodata, position=[0.1,0.2,0.5,0.9]  $
		, 	self.Xrange, self.Yrange $
		, 	/xstyle, /ystyle $
		, 	background=255, color=0 $
		, 	xtitle = self.xtitle, ytitle=self.ytitle $
		, 	charsize=1.5, charthick=2.0, xthick=2.0, ythick=2.0

		loadct, 12

		for i=0L, n_elements(*self.X)-1 do begin
			oplot, *(*self.X)[i], *(*self.Y)[i]	, color=6*16	, linestyle = 0, thick=2
			oplot, *(*self.X)[i], *(*self.F)[i]	, color=12*16	, linestyle = 0, thick=2
		endfor

		top=0.9 & dy=0.05 & x0=0.525 & charsize=1.5 & charthick=2.0

		xyouts, x0, top-0*dy, self.title, color=0, /normal, charsize=charsize, charthick=charthick

		for i=0L, n_elements(*self.P)-1 do begin
			P = *(*self.P)[i]
			for j=0L, n_elements(P)-1 do xyouts, $
			 	x0, top-dy*(3+P[j].Nr), $
			 	P[j].Name + ' = ' + PMI__Round(P[j].Value,3,/string) + ' ' + P[j].Units, $
			 	color=0, /normal, charsize=charsize, charthick=charthick
		endfor

		loadct, 0
	endif

END




PRO PMI__Display__RoiFit::GET, CursorPos = CursorPos

	if arg_present(CursorPos) then CursorPos=self.CursorPos

END


PRO PMI__Display__RoiFit::Cleanup
	widget_control, self.id, /destroy
	ptr_free, *self.X, *self.Y, *self.F, *self.P
	ptr_free, self.X, self.Y, self.F, self.P
END

FUNCTION PMI__Display__RoiFit::Init, parent, CursorPos, xsize=xsize, ysize=ysize

	self.CursorPos = CursorPos

	self.id = widget_base(parent,/column,map=0,event_func='PMI__Display__Event__RoiFit')

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

PRO PMI__Display__RoiFit__Define

	Struct = {PMI__Display__RoiFit, $
		id:0L, DrawId:0L, CursorPos:lonarr(4), $
		title:'', xtitle:'', ytitle:'', xrange: fltarr(2), yrange: fltarr(2), $
		X:ptr_new(),Y:ptr_new(),F:ptr_new(),P:ptr_new() }
END

