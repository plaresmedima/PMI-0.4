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


PRO PMI__DISPLAY__2D1DVIEW::EVENT, ev

	widget_control, ev.id, get_uvalue=module

	IF module EQ self.Lists THEN BEGIN
		Self.Draw -> GET, DimSlice=OldDim
		Self.Lists -> Get, Stdy=Stdy, Series=Series, Region=Region
		self.Draw -> Set, Stdy=Stdy, Series=Series, Region=Region, /LoadData, /REFRESH, /ERASE
		Self.Plot -> Set, LoadData = ev.list ne 2, /REFRESH
		self.Tools -> Set, /REFRESH
		IF ev.list NE 2 THEN BEGIN
			Self.Draw -> GET, DimSlice=NewDim
			self.Draw -> SET, RESETZOOM = total(OldDim eq NewDim) lt 2
			self.Navigate -> SET, /Refresh
			self.Colors -> SET, /Refresh
		ENDIF
		self.Draw -> Set, /LoadData, /REFRESH, /ERASE
		IF ev.list EQ 0 THEN BEGIN
			PMI__INFO, ev.top, STATE=s
			s->sel, ev.index-1
		ENDIF ELSE IF obj_valid(Stdy) THEN Stdy->sel, ev.list-1, ev.index-1
		PMI__control, ev.top, /MenuRefresh
	ENDIF

	IF module EQ self.Navigate THEN Self.Plot -> Set, LoadData = ev.dim eq 0, /Refresh

	IF module EQ self.Draw THEN BEGIN
		self.Draw->GET, Tool=Tool, Mode=Mode
		UpdateLists=0
		IF (Mode eq 'NEW') THEN CASE Tool OF
			'RegionDisplay'	: UpdateLists = ev.type eq 1
			'RegionCircle'	: UpdateLists = ev.type eq 1
			'RegionRectangle':UpdateLists = ev.type eq 1
			'RegionPolygon'	: UpdateLists = (ev.type eq 0) and (ev.clicks eq 2)
			'RegionPixel'	: UpdateLists = (ev.type eq 0) and (ev.clicks eq 2)
			ELSE:
			ENDCASE
		IF UpdateLists THEN BEGIN
			self.lists -> SET, /INIT, /REFRESH
			self.Tools -> SET, /Refresh
			PMI__Control, ev.top, /MenuRefresh
		ENDIF
		IF ev.type EQ 6 THEN self.Navigate -> SET, /Refresh
		IF ev.key EQ 7 $
		OR ev.key EQ 8 THEN self.Plot -> Set, /LoadData
		Self.Plot -> Set, /Refresh
	ENDIF

	self -> Set, /DefaultMsg
END
PRO PMI__DISPLAY__2D1DVIEW__EVENT, ev
	widget_control, ev.handler, get_uvalue=self
	self -> event, ev
END


PRO PMI__DISPLAY__2D1DVIEW::Set, $
	PMI__REFRESH=pmi__refresh, PMI__RESIZE=pmi_resize, $
	xsize=xsize,ysize=ysize, $
	DefaultMsg=DefaultMsg

	if keyword_set(pmi__refresh) then begin
		self.Lists -> SET, /INIT, /REFRESH
		self.Lists -> GET, STDY=st, SERIES=se, REGION=re
		self.Draw -> SET, STDY=st, SERIES=se, REGION=re, /LOADDATA, /REFRESH, /ERASE
		self.Plot -> Set, /LOADDATA, /REFRESH
		self.Tools -> Set, /REFRESH
		self.Colors -> Set, /REFRESH
		self.Navigate -> Set, /REFRESH
		self -> Set, /DefaultMsg
	endif
	if keyword_set(pmi_resize) then begin
		gpmi = widget_info(/geometry, tlb(self.id))
		self -> Set, xsize=gpmi.xsize, ysize=gpmi.ysize
		self.Draw -> Set, /Refresh, /Erase
		self.Plot -> Set, /Refresh
	endif


	if n_elements(xsize) ne 0 $
	or n_elements(ysize) ne 0 then begin
		widget_control, self.id, xsize=xsize, ysize=ysize
		self.Lists -> Set, xsize=xsize-345
		self.Draw -> Set, xsize=xsize/2, ysize=ysize-135
		self.Plot -> Set, xsize=xsize/2, ysize=ysize-135
	endif

	if keyword_set(DefaultMsg) then begin
		self.Draw -> Get, PixelValue = pv
		self.Message -> set, PixelValue = pv
	endif

END
PRO PMI__DISPLAY__2D1DVIEW::Get, CursorPos=CursorPos

	if arg_present(CursorPos) then self.Draw->Get, CursorPos=CursorPos
END



PRO PMI__DISPLAY__2D1DVIEW::Cleanup

	obj_destroy, [Self.Draw, Self.Plot, Self.Tools, Self.Navigate, Self.Colors, Self.Lists, Self.Message]
	widget_control, self.id, /destroy
END
FUNCTION PMI__DISPLAY__2D1DVIEW::Init, parent, CursorPos, xsize=xsize,ysize=ysize

	self.id = widget_base(parent,/column,uname='PMI__Display',map=0,event_pro='PMI__DISPLAY__2D1DVIEW__EVENT')

	;BUILD INTERFACE

	controls_ysize=95
	id = widget_base(self.id,/row,/base_align_center)
	self.Tools = Obj_new('PMI__Module__CursorTools',id,ysize=controls_ysize,/zoomin,/region)
	self.Navigate = Obj_new('PMI__Module__4DNavigator',id,xsize=170,ysize=controls_ysize)
	self.Colors = Obj_new('PMI__Module__ColorSettings',id,ysize=controls_ysize)
	self.Lists = Obj_new('PMI__Module__ViewDataLists',id,ysize=controls_ysize)
	self.Message = Obj_new('PMI__Module__MessageBar',self.id)
	id = widget_base(self.id,/row,/base_align_center)
	self.Draw = Obj_new('PMI__Module__StandardDraw',id, CursorPos=CursorPos)
	self.Plot = Obj_new('PMI__Module__PixelPlot',id)
	self -> Set, xsize=xsize,ysize=ysize

	;INITIALIZE

	self.Lists -> SET, /INIT, /REFRESH
	self.Lists -> GET, STDY=st, SERIES=se, REGION=re
	self.Draw -> SET, STDY=st, SERIES=se, REGION=re, /LOADDATA, /REFRESH
	self.Plot -> SET, MASTER=self.Draw, /LOADDATA, /REFRESH
	self.Tools -> SET, MASTER=self.Draw, /REFRESH
	self.Navigate -> SET, MASTER=self.Draw, /REFRESH
	self.Colors -> SET, MASTER=self.Draw, /REFRESH
	self -> SET, /DefaultMsg

	widget_control, self.id, set_uvalue = self, /map
	return, 1
END

PRO PMI__DISPLAY__2D1DVIEW__Define

	PMI__Module__CursorTools__Define
	PMI__Module__4DNavigator__Define
	PMI__Module__ColorSettings__Define
	PMI__Module__ViewDataLists__Define
	PMI__Module__StandardDraw__Define
	PMI__Module__PixelPlot__Define
	PMI__Module__MessageBar__Define

	struct = {PMI__DISPLAY__2D1DVIEW,$
		id			:0L,$
		Draw		:obj_new(),$
		Plot		:obj_new(),$
		Navigate	:obj_new(),$
	 	Lists		:obj_new(),$
		Colors		:obj_new(),$
		Tools		:obj_new(),$
		Message		:obj_new()}
END