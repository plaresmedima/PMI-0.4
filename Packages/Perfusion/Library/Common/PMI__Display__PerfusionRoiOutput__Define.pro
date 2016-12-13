;
;
;    Copyright (C) 2009 Steven Sourbron
;
;    This program is free software; you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation; either version 2 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License along
;    with this program; if not, write to the Free Software Foundation, Inc.,
;    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
;
;
;

FUNCTION PMI__Display__PerfusionRoiOutput::Event, ev

	CASE widget_info(ev.id,/uname) of

		'Save':begin
			PMI__Info, ev.top, Stdy=Stdy
			Path = Stdy->Datapath() + self.Roiname + '__'
			Write_tiff, Path + self.model + '.tif', reverse(tvrd(/true),3)
			PMI__WritePlot, Path + 'Fit__' + self.model + '.txt', *self.time, *self.fit
			PMI__WritePlot, Path + 'Curve.txt', *self.time, *self.Curve
			if ptr_valid(self.aif) then PMI__WritePlot, Path + 'Aif.txt', *self.time, *self.aif
			end

		'Save As':begin
			PMI__Info, ev.top, State=State
			if not State->get_file(file, file=cleanstr(self.Roiname + '__' + self.model), title='Save plot as..', filter='.tif') then return,0B
			Write_tiff, file, reverse(tvrd(/true),3)
			filename = strmid(file,0,strlen(file)-4)
			PMI__WritePlot, filename + '__Curve.txt', *self.time, *self.Curve
			PMI__WritePlot, filename + '__Fit.txt', *self.time, *self.Fit
			if ptr_valid(self.aif) then PMI__WritePlot, filename + '__Aif.txt', *self.time, *self.Aif
			end

		'Close':begin
			PMI__Control, ev.top, Viewer = 'PMI__DISPLAY__2DVIEW'
			PMI__Control, ev.top, /refresh
			end

	ENDCASE

	return, 0B
END

FUNCTION PMI__Display__Event__PerfusionRoiOutput, ev

	widget_control, ev.handler, get_uvalue=self
	return, Self -> Event(ev)
END






PRO PMI__Display__PerfusionRoiOutput::SET, $
	PMI__REFRESH=pmi__refresh, PMI__RESIZE=pmi_resize, $
	Refresh=Refresh, $
 	xsize=xsize, ysize=ysize, $
 	Style = Style, $
	Model = Model, $
	Time = Time, $
	Curve = Curve, $
	Aif = Aif, $
	Fit = Fit, $
	Units = Units, $
	RoiName = RoiName, $
	Parameters = Parameters

	if keyword_set(pmi_resize) then begin
		gpmi = widget_info(/geometry, tlb(self.id))
		self -> Set, xsize=gpmi.xsize, ysize=gpmi.ysize, /REFRESH
	endif

	if n_elements(Style) 		ne 0 then self.Style = Style
	if n_elements(Model) 		ne 0 then self.Model = Model
	if n_elements(Time) 		ne 0 then self.Time = ptr_new(Time)
	if n_elements(Curve) 		ne 0 then self.Curve = ptr_new(Curve)
	if n_elements(Aif) 			ne 0 then self.Aif = ptr_new(Aif)
	if n_elements(Fit) 			ne 0 then self.Fit = ptr_new(Fit)
	if n_elements(Units) 		ne 0 then self.Units = Units
	if n_elements(RoiName) 		ne 0 then self.RoiName = RoiName
	if n_elements(Parameters) 	ne 0 then self.Parameters = ptr_new(Parameters)

	if n_elements(xsize) ne 0 $
	or n_elements(ysize) ne 0 then widget_control, self.DrawId, xsize=xsize, ysize=ysize-40

	if keyword_set(Refresh) then begin

		widget_control, self.DrawId, get_value = win
		wset, win & erase
		if not ptr_valid(self.Curve) then return

		n = n_elements(*self.Curve)
		MinCurve = min(*self.Curve,max=MaxCurve)
		MinFit = min(*self.Fit,max=MaxFit)

 		plot, /nodata, position=[0.1,0.2,0.5,0.9]  $
		, 	[0,(*self.time)[n-1]], [min([MinCurve,MinFit]),max([MaxCurve,MaxFit])] $
		, 	/xstyle, /ystyle $
		, 	background=255, color=0 $
		, 	xtitle = 'Time (sec)', ytitle=self.Units $
		, 	charsize=1.5, charthick=2.0, xthick=2.0, ythick=2.0

		loadct, 12

		case self.style of
		0: oplot, *self.time, *self.curve	, color=6*16	, linestyle = 0, thick=2
		1: oplot, *self.time, *self.curve	, color=6*16	, psym = 4, thick=2, symsize=2
		endcase

		oplot, *self.time, *self.Fit	, color=12*16	, linestyle = 0, thick=2

		case self.style of
		0:
		1: oplot, *self.time, *self.Fit	, color=12*16, psym = 4, thick=2, symsize=2
		endcase

		top=0.9 & dy=0.05 & x0=0.525 & charsize=1.5 & charthick=2.0

		xyouts, x0, top-0*dy, 'Region: ' + self.RoiName	, color=0, /normal, charsize=charsize, charthick=charthick
		xyouts, x0, top-1*dy, 'Model: ' + self.Model	, color=0, /normal, charsize=charsize, charthick=charthick

		P = *self.Parameters

		for i=0L,n_elements(P)-1 do xyouts $
			, x0, top-dy*(3+P[i].Nr) $
			, P[i].Name + ' = ' + PMI__Round(P[i].Value,P[i].Rnd,/string) + ' ' + P[i].Units $
			, color=0, /normal, charsize=charsize, charthick=charthick

		loadct, 0
	endif

END



PRO PMI__Display__PerfusionRoiOutput::GET, CursorPos = CursorPos

	if arg_present(CursorPos) then CursorPos=self.CursorPos

END


PRO PMI__Display__PerfusionRoiOutput::Cleanup
	widget_control, self.id, /destroy
	ptr_free, self.Parameters, self.Time, self.Curve, self.Fit, self.Aif
END

FUNCTION PMI__Display__PerfusionRoiOutput::Init, parent, CursorPos, xsize=xsize, ysize=ysize

	self.CursorPos = CursorPos

	self.id = widget_base(parent,/column,map=0,event_func='PMI__Display__Event__PerfusionRoiOutput')

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

PRO PMI__Display__PerfusionRoiOutput__Define

	Struct = {PMI__Display__PerfusionRoiOutput 	$
	,	id: 0L 	$
	,	DrawId:0L $
	,	CursorPos:lonarr(4)	$
	,	Parameters: ptr_new() $
	,	Model:'' $
	,	RoiName:'' $
	,	Units: '' $
	,	Time:ptr_new() $
	,	Curve: ptr_new()$
	,	Aif:ptr_new() $
	,	Fit:ptr_new() $
	,	Style: 0B $ (0: connect data with lines, 1: plot data with symbol)
	}
END

