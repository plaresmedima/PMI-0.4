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


PRO PMI__Module__RoiPlot::SET $
, 	xsize=xsize, ysize=ysize $
, 	Refresh=Refresh $
, 	Xaxis=Xaxis, Yaxis=Yaxis $
, 	psym=psym, symsize=symsize $
, 	charsize=charsize, thick=thick $
, 	title=title, xtitle=xtitle, ytitle=ytitle $
, 	MinX=MinX, MaxX=MaxX, MinY=MinY, MaxY=MaxY $
, 	Initialize=Initialize

	if n_elements(xsize) ne 0 $
	or n_elements(ysize) ne 0 $
	then widget_control, self.id, scr_xsize=xsize, scr_ysize=ysize

	if n_elements(Xaxis) ne 0 then begin
		ptr_free, self.X
		self.X = ptr_new(Xaxis)
	endif
	if n_elements(Yaxis) ne 0 then begin
		ptr_free, self.Y
		self.Y = ptr_new(Yaxis)
	endif

	if n_elements(psym) 		ne 0 then self.psym = psym
	if n_elements(charsize) 	ne 0 then self.charsize = charsize
	if n_elements(symsize) 		ne 0 then self.symsize = symsize
	if n_elements(thick) 		ne 0 then self.thick = thick
	if n_elements(title) 		ne 0 then self.title = title
	if n_elements(xtitle) 		ne 0 then self.xtitle = xtitle
	if n_elements(ytitle) 		ne 0 then self.ytitle = ytitle
	if n_elements(MinX) 		ne 0 then self.MinX = MinX
	if n_elements(MaxX) 		ne 0 then self.MaxX = MaxX
	if n_elements(MinY) 		ne 0 then self.MinY = MinY
	if n_elements(MaxY) 		ne 0 then self.MaxY = MaxY

	if keyword_set(Initialize) then begin
		self->GET, Xaxis=X, Yaxis=Y
		self.MinX=min(X) & self.MaxX=max(X)
		self.MinY=min(Y) & self.MaxY=Max(Y)
	    self.psym=0B & self.symsize=1E
    	self.charsize=1.5 & self.thick=1.5
	endif

	if keyword_set(Refresh) then begin

		widget_control, self.id, get_value = win
		wset, win & loadct, 0
		self -> GET, Xaxis=X, Yaxis=Y
		if n_elements(Y) gt 1 then begin
			plot, X, Y, /xstyle, /ystyle $
			, 	background=255, color=0 $
			, 	psym=self.psym, symsize=self.symsize $
			, 	charsize=self.charsize, charthick=self.thick $
			, 	thick=self.thick, xthick=self.thick, ythick=self.thick $
			, 	title=self.title, xtitle=self.xtitle, ytitle=self.ytitle $
			, 	yrange=[self.MinY,self.MaxY] $
			, 	xrange=[self.MinX,self.MaxX]
		endif else erase
	endif
END

PRO PMI__Module__RoiPlot::GET $
, 	Xaxis=Xaxis, Yaxis=Yaxis $
, 	psym=psym, symsize=symsize $
, 	charsize=charsize, thick=thick $
, 	title=title, xtitle=xtitle, ytitle=ytitle $
, 	MinX=MinX, MaxX=MaxX $
, 	MinY=MinY, MaxY=MaxY

	if arg_present(Yaxis) then begin
		if ptr_valid(self.Y) then Yaxis=*self.Y
	endif
	if arg_present(Xaxis) then begin
		if ptr_valid(self.X) then Xaxis=*self.X $
		else if ptr_valid(self.Y) then Xaxis=findgen(n_elements(*self.Y))
	endif

	if arg_present(psym) 		then psym = self.psym
	if arg_present(charsize) 	then charsize = self.charsize
	if arg_present(symsize) 	then symsize = self.symsize
	if arg_present(thick) 		then thick = self.thick
	if arg_present(title) 		then title = self.title
	if arg_present(xtitle) 		then xtitle = self.xtitle
	if arg_present(ytitle) 		then ytitle = self.ytitle
	if arg_present(MinX) 		then MinX = self.MinX
	if arg_present(MaxX) 		then MaxX = self.MaxX
	if arg_present(MinY) 		then MinY = self.MinY
	if arg_present(MaxY) 		then MaxY = self.MaxY
END



PRO PMI__Module__RoiPlot::Cleanup
	ptr_free, self.X, self.Y
END
FUNCTION PMI__Module__RoiPlot::Init, parent, uname=uname

	self.id	= widget_draw(parent,/retain, uname=uname)
	widget_control, self.id, set_uvalue = self

	self.psym=4B & self.symsize=1E
   	self.charsize=1.5 & self.thick=1.5

	return, 1B
END
PRO PMI__Module__RoiPlot__Define

	struct = {PMI__Module__RoiPlot $
	,	id:0L $
	,	X:ptr_new(), Y:ptr_new() $
    ,	MinX:0E, MaxX:0E $
    ,	MinY:0E, MaxY:0E $
    ,	psym:0B, symsize:1E $
    , 	charsize:1E, thick:1E $
    ,	title:'', xtitle:'', ytitle:'' $
	}

END