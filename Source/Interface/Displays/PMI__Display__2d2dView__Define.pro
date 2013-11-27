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


PRO PMI__Display__2d2dView::Event, ev

	widget_control, ev.id, get_uvalue=module

	IF (module EQ self.navigate) $
	OR (module EQ self.colors) $
	THEN BEGIN
		Self.Draw[self.focus] -> Set, /Focus
		Self.Draw[1-self.Focus] -> Set, /Refresh
		self -> Get, PlotLines=PlotLines
		self.Draw[self.focus] -> Set, PlotLine=PlotLines[*,self.focus]
		self.Draw[1-self.focus] -> Set, PlotLine=PlotLines[*,1-self.focus]
	ENDIF

	;A button click in the draw widget that is out of focus changes the focus to that draw widget
	IF module EQ self.Draw[1-self.Focus] THEN BEGIN
		IF ev.type EQ 0 THEN BEGIN
			self.Focus = 1-self.Focus
			self.Draw[self.Focus] -> Get, STDY=st, SERIES=se, REGION=re
			self.Lists -> Set, STDY=st, SERIES=se, REGION=re, /REFRESH
			self.Tools -> Set, MASTER=self.Draw[self.Focus], /REFRESH
			self.Colors -> Set, MASTER=self.Draw[self.Focus], /REFRESH
			self.Navigate -> Set, MASTER=self.Draw[self.Focus], /REFRESH
			Self.Draw[self.Focus] -> Set, /Focus
			Self.Draw[1-self.Focus] -> Set, /Refresh, /erase
			self -> Get, PlotLines=PlotLines
			self.Draw[0] -> Set, PlotLine=PlotLines[*,0]
			self.Draw[1] -> Set, PlotLine=PlotLines[*,1]
		ENDIF
	ENDIF

	IF module EQ self.draw[self.focus] THEN BEGIN
		self.draw[self.focus]->GET, Tool=Tool, Mode=Mode
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
		IF Tool EQ 'Scroll_z' $
		OR Tool EQ 'Scroll_t' $
		OR ev.type EQ 6 THEN self.Navigate -> SET, /REFRESH
		self.draw[self.focus] -> SET, /FOCUS
		self -> Get, PlotLines=PlotLines
		self.Draw[0] -> Set, PlotLine=PlotLines[*,0]
		self.Draw[1] -> Set, PlotLine=PlotLines[*,1]
	ENDIF

	IF module EQ self.Lists THEN BEGIN
		Self.Draw[self.focus] -> GET, DimSlice=OldDim
		Self.Lists -> Get, STDY=Stdy, SERIES=Series, REGION=Region
		self.Draw[self.focus] -> Set, STDY=Stdy, SERIES=Series, REGION=Region
		Self.Draw[1-self.focus] -> Set, /Refresh, /erase
		IF ev.list NE 2 THEN BEGIN
			Self.Draw[self.focus] -> GET, DimSlice=NewDim
			self.Draw[self.focus] -> SET, RESETZOOM = total(OldDim eq NewDim) lt 2
			self.Navigate -> SET, /Refresh
			self.Colors -> SET, /Refresh
		ENDIF
		self.Draw[self.focus] -> Set, /LoadData, /Refresh, /erase, /Focus
		self -> Get, PlotLines=PlotLines
		self.Draw[0] -> Set, PlotLine=PlotLines[*,0]
		self.Draw[1] -> Set, PlotLine=PlotLines[*,1]
		IF ev.list EQ 0 THEN BEGIN
			PMI__INFO, ev.top, STATE=s
			s->sel, ev.index-1
		ENDIF ELSE IF obj_valid(Stdy) THEN Stdy->sel, ev.list-1, ev.index-1
		PMI__control, ev.top, /MenuRefresh
	ENDIF

	self -> Set, /DefaultMsg
END

PRO PMI__Display__2d2dView__Event, ev
	widget_control, ev.handler, get_uvalue=self
	self -> event, ev
END


PRO PMI__Display__2d2dView::Set, $
	PMI__REFRESH=pmi__refresh, $
	PMI__RESIZE=pmi_resize, $
	XSIZE=xsize, YSIZE=ysize, $
	DEFAULTMSG=DefaultMsg

	if keyword_set(pmi__refresh) then begin
		Self.Lists -> Set, /INIT, /REFRESH
		Self.Lists -> Get, STDY=st, SERIES=se, REGION=re
		self.Draw[self.focus] -> Set, STDY=st, SERIES=se, REGION=re, /LoadData, /erase, /Refresh, /Focus
		self.Draw[1-self.focus] -> Set, /LoadData, /Refresh
		self -> Get, PlotLines=PlotLines
		self.Draw[0] -> Set, PlotLine=PlotLines[*,0]
		self.Draw[1] -> Set, PlotLine=PlotLines[*,1]
		self.Tools -> Set, /Refresh
		self.Colors -> Set, /Refresh
		self.Navigate -> Set, /Refresh
		self -> Set, /DefaultMsg
	endif
	if keyword_set(pmi_resize) then begin
		gpmi = widget_info(/geometry, tlb(self.id))
		self -> Set, xsize=gpmi.xsize, ysize=gpmi.ysize
		Self.Draw[self.focus] -> Set, /Refresh, /erase, /Focus
		Self.Draw[1-self.focus] -> Set, /Refresh, /erase
		self -> Get, PlotLines=PlotLines
		self.Draw[0] -> Set, PlotLine=PlotLines[*,0]
		self.Draw[1] -> Set, PlotLine=PlotLines[*,1]
	endif

	if n_elements(xsize) ne 0 $
	or n_elements(ysize) ne 0 then begin
		widget_control, self.id, xsize=xsize, ysize=ysize
		self.Lists -> Set, xsize=xsize-345
		self.Draw[0] -> Set, xsize=(xsize-20)/2, ysize=ysize-135
		self.Draw[1] -> Set, xsize=(xsize-20)/2, ysize=ysize-135
	endif

	if keyword_set(DefaultMsg) then begin
		self.Draw[self.Focus] -> Get, PixelValue = pv
		self.Message -> set, PixelValue = pv
	endif
END

PRO PMI__Display__2d2dView::Get, CursorPos=CursorPos, PlotLines=PlotLines

	IF arg_present(CursorPos) THEN self.Draw[self.Focus]->Get, CursorPos=CursorPos

	IF arg_present(PlotLines) THEN BEGIN

		PlotLines = fltarr(3,2)

		self.Draw[0] -> GET, SERIES=s0, CursorPos=P0
		self.Draw[1] -> GET, SERIES=s1, CursorPos=P1
		if obj_valid(s0) and obj_valid(s1) then begin
			P0 = reform_ind([s0->d(2),s0->d(3)], vec=P0[2:3])
			P1 = reform_ind([s1->d(2),s1->d(3)], vec=P1[2:3])
			PlotLines[*,0] = s0->LocalizerLine(P0,s1,P1)
			PlotLines[*,1] = s1->LocalizerLine(P1,s0,P0)
		endif
	ENDIF
END



PRO PMI__Display__2d2dView::Cleanup
	obj_destroy, [Self.Draw, Self.Tools, Self.Navigate, Self.Colors, Self.Lists, Self.Message]
	widget_control, self.id, /destroy
END
FUNCTION PMI__Display__2d2dView::Init, parent, CursorPos, xsize=xsize, ysize=ysize

	self.id = widget_base(parent,/column,map=0,event_pro='PMI__Display__2d2dView__EVENT')

	;BUILD INTERFACE

	controls_ysize=95
	id = widget_base(self.id,/row,/base_align_center)
	self.Tools = Obj_new('PMI__Module__CursorTools',id,ysize=controls_ysize,/all)
	self.Navigate = Obj_new('PMI__Module__4DNavigator',id,xsize=150,ysize=controls_ysize)
	self.Colors = Obj_new('PMI__Module__ColorSettings',id,ysize=controls_ysize)
	self.Lists = Obj_new('PMI__Module__ViewDataLists',id,ysize=controls_ysize)
	self.Message = Obj_new('PMI__Module__MessageBar',self.id)
	id = widget_base(self.id,/row,/base_align_center)
	self.Draw[0] = Obj_new('PMI__Module__StandardDraw',id, CursorPos=CursorPos)
	self.Draw[1] = Obj_new('PMI__Module__StandardDraw',id, CursorPos=CursorPos)
	self -> Set, xsize=xsize, ysize=ysize

	;INITIALIZE

	self.Lists -> Set, /INIT, /REFRESH
	self.Lists -> Get, STDY=st, SERIES=se, REGION=re
	self.Draw[self.focus] -> Set	, STDY=st, SERIES=se, REGION=re, /LOADDATA, /REFRESH, /FOCUS
	self.Draw[1-self.focus] -> Set	, STDY=st, SERIES=se, REGION=re, /LOADDATA, /REFRESH
	self -> Get, PlotLines=PlotLines
	self.Draw[0] -> Set, PlotLine=PlotLines[*,0]
	self.Draw[1] -> Set, PlotLine=PlotLines[*,1]
	self.Tools -> Set, MASTER=self.Draw[self.focus], /REFRESH
	self.Navigate -> Set, MASTER=self.Draw[self.focus], /REFRESH
	self.Colors -> Set, MASTER=self.Draw[self.focus], /REFRESH
	self -> Set, /DEFAULTMSG

	widget_control, self.id, set_uvalue = self, /map
	return, 1
END
PRO PMI__Display__2d2dView__Define

	PMI__Module__CursorTools__Define
	PMI__Module__4DNavigator__Define
	PMI__Module__ColorSettings__Define
	PMI__Module__ViewDataLists__Define
	PMI__Module__StandardDraw__Define
	PMI__Module__MessageBar__Define

	struct = {PMI__Display__2d2dView 	$
	,	id			:0L 	$
	,	Focus		:0B		$ The index of the draw widget in focus (Left=0 or Right=1)
	,	Draw		:objarr(2) $
	,	Navigate	:obj_new() $
	, 	Lists		:obj_new() $
	,	Colors		:obj_new() $
	,	Tools		:obj_new() $
	,	Message		:obj_new() $
	}
END