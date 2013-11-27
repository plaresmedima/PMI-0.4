;
;    Copyright (C) 2013 Steven Sourbron
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
;    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.


PRO PMI__Module__PixelPlot__Event, ev
END




PRO PMI__Module__PixelPlot::Set $
, MASTER = master $
, xsize = xsize $
, ysize = ysize $
, LoadData = LoadData $
, Refresh = Refresh $
, Win = Win $
, Message = Message $
, Erase = Erase

	if n_elements(master) ne 0 then self.master=master
	if n_elements(xsize) ne 0 or n_elements(ysize) ne 0 then widget_control, self.id, scr_xsize=xsize, scr_ysize=ysize
	if keyword_set(Win) then begin
		widget_control, self.id, get_value = win
		wset, win
		loadct, 0
	endif
	if n_elements(Message) ne 0 then begin
		self->Set, /Win
		erase, 255
		g = widget_info(self.id,/geometry)
		xyouts, 30, g.scr_ysize/2, Message, /Device, Color=0, charsize=2
	endif
	if keyword_set(LoadData) then begin
		ptr_free, self.raw
		Self.master->Get, Series=Series, Stdy=Stdy, CursorPos=CursorPos
		if not obj_valid(Series) then return
		Self->Set, Message = 'LOADING DATA...'
		raw = Series->Read(Stdy->DataPath(),CursorPos[2],-1)
		self.raw = ptr_new(raw,/no_copy)
	endif
	if keyword_set(Refresh) then begin
		self->Set, /Win
		if ptr_valid(self.raw) then begin
			Self.master->Get, Series=Series, CursorPos=CursorPos
			if Series->d(3) gt 1 then begin
				t = Series->t()
				y = reform((*self.raw)[CursorPos[0],CursorPos[1],*])
				plot, t-t[0], y, background=255, color=0, xtitle='Time (s)', ytitle=Series->name(), /xstyle
				oplot, [t[CursorPos[3]]-t[0]], [y[CursorPos[3]]], psym=4, color=0, thick=2
			endif else erase
		endif else erase
	endif
END



PRO PMI__Module__PixelPlot::Cleanup
	ptr_free, self.raw
END
FUNCTION PMI__Module__PixelPlot::Init, parent

	self.id	= widget_draw(parent,/retain,event_pro='PMI__Module__PixelPlot__Event',/button_events,/motion_events)
	widget_control, self.id, set_uvalue = self
	return, 1B
END
PRO PMI__Module__PixelPlot__Define

	struct = {PMI__Module__PixelPlot $
	,	id		:0L 	$
	,	master 	:obj_new() $
	,	raw		:ptr_new() $	;3D Pixel data
	}

END