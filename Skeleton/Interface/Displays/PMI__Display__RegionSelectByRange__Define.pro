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



PRO PMI__DISPLAY__REGIONSELECTBYRANGE::EVENT, ev, Button=Button

	if keyword_set(button) then begin
		case widget_info(ev.id,/uname) of
		'All slices':begin
			self.Range -> GET, Range=r
			self.Draw -> Get, Stdy=Stdy, Series=Series, Region=Region
			d = Series -> d()
 			for k=0L,d[2]*d[3]-1 do begin
 				self.Message -> set, 'Creating ROI', k/(d[2]*d[3]-1E)
 				Im = Series->Read(Stdy->DataPath(),k)
 				bin = (Im ge r[0]) and (Im le r[1])
 				Region -> Write, Stdy->DataPath(), bin, k
 			endfor
 			self -> SET, /DEFAULTMSG
		end
		'This slice':begin
			self.Draw -> Get, Stdy=Stdy, Series=Series, Region=Region
			d = Series -> d()
			bin = bytarr(d[0]*d[1])
 			for k=0L,d[2]*d[3]-1 do begin
 				self.Message -> set, 'Clearing ROI', k/(d[2]*d[3]-1E)
 				Region -> Write, Stdy->DataPath(), bin, k
 			endfor
 			self.Range -> SET, /PIXELS
 			self -> SET, /DEFAULTMSG
		end
		'Close':begin
			Menu = widget_info(widget_info(ev.top,/child),/all_children)
			for i=0L,n_elements(Menu)-1 do widget_control, Menu[i], /sensitive
			PMI__Control, ev.top, Viewer = 'PMI__DISPLAY__2DVIEW'
			PMI__Control, ev.top, /refresh
			end
		endcase
		return
	endif

	widget_control, ev.id, get_uvalue=module

	IF module EQ self.Lists THEN BEGIN
		self.Lists -> Get, Stdy=Stdy, Series=Series, Region=Region
		self.Draw -> SET, Stdy=Stdy, Series=Series, Region=Region, /LoadData, /Refresh, /Erase
		IF ev.list NE 2 THEN BEGIN
			self.Navigate -> SET, /Refresh
			self.Colors -> SET, /Refresh
			self -> SET, /DefaultMsg
		ENDIF
		IF ev.list EQ 0 THEN BEGIN
			PMI__INFO, ev.top, STATE=s
			s->sel, ev.index-1
		ENDIF ELSE IF obj_valid(Stdy) THEN Stdy->sel, ev.list-1, ev.index-1
	ENDIF

	self.Range -> SET, /PIXELS
	self -> SET, /DefaultMsg, /HISTOGRAM
END

PRO PMI__DISPLAY__REGIONSELECTBYRANGE__EVENT, ev
	widget_control, ev.handler, get_uvalue=self
	self -> event, ev
END
PRO PMI__DISPLAY__REGIONSELECTBYRANGE__BTTNEVENT, ev
	widget_control, widget_info(widget_info(ev.handler,/parent),/parent), get_uvalue=self
	self -> event, ev, /button
END


PRO PMI__DISPLAY__REGIONSELECTBYRANGE::Set, $
	PMI__REFRESH=pmi__refresh, PMI__RESIZE=pmi_resize, $
 	XSIZE=xsize, YSIZE=ysize, $
 	HISTOGRAM=histogram, $
 	DefaultMsg = DefaultMsg

	if keyword_set(pmi__refresh) then begin
		self.Lists -> SET, /INIT, /REFRESH
		self.Lists -> GET, STDY=st, SERIES=se, REGION=re
		self.Draw -> SET, STDY=st, SERIES=se, REGION=re, /LOADDATA, /REFRESH, /ERASE
		self.Navigate -> SET, /REFRESH
		self.Range -> SET, /REFRESH
		self.Colors -> SET, /REFRESH
		self -> SET, /DEFAULTMSG
	endif
	if keyword_set(pmi_resize) then begin
		gpmi = widget_info(/geometry, tlb(self.id))
		self -> Set, xsize=gpmi.xsize, ysize=gpmi.ysize
		self.Draw -> Set, /Refresh, /Erase
		self->SET, /HISTOGRAM
	endif

	if n_elements(xsize) ne 0 $
	or n_elements(ysize) ne 0 then begin
		widget_control, self.id, xsize=xsize, ysize=ysize
		self.Lists -> Set, xsize=xsize - 455
		self.Draw -> Set, xsize=(xsize-20)/2, ysize=ysize-135
		self.Plot -> Set, xsize=(xsize-20)/2, ysize=ysize-135
	endif

	if keyword_set(Histogram) then begin
		self.Draw -> Get, Series=S, Region=R, Raw=Im
		if obj_valid(S) and obj_valid(R) then begin
			self.Range->GET, Range=r
 			ind = where( (Im ge r[0]) and (Im le r[1]), cnt)
 			if cnt ge 2 then begin
 				v = Im[ind]
 				nbins = min([100,ceil(cnt/10E)])
 				minb = min(v,max=maxb)
 				binsize = (maxb-minb)/nbins
				yhist = histogram(v,min=minb,max=maxb,binsize=binsize)
				xhist = binsize*findgen(nbins) + minb + binsize/2
				self.Plot->SET, Xaxis=xhist, Yaxis=yhist, minX=min(xhist), maxX=max(xhist), minY=0, maxY=max(yhist), /REFRESH
 			endif else self.Plot->SET, Xaxis=0, Yaxis=0, /REFRESH
 		endif else self.Plot->SET, Xaxis=0, Yaxis=0, /REFRESH
	endif

	if keyword_set(DefaultMsg) then begin
		self.Draw -> Get, PixelValue = pv
		self.Message -> set, PixelValue = pv
	endif
END
PRO PMI__DISPLAY__REGIONSELECTBYRANGE::GET, $
	CursorPos=CursorPos

	if arg_present(CursorPos) then self.Draw->Get, CursorPos=CursorPos
END




PRO PMI__DISPLAY__REGIONSELECTBYRANGE::Cleanup

	obj_destroy, [Self.Draw, self.plot, Self.Navigate, self.Range, Self.Colors, Self.Lists, Self.Message]
	widget_control, self.id, /destroy
END
FUNCTION PMI__DISPLAY__REGIONSELECTBYRANGE::Init, parent, CursorPos, xsize=xsize,ysize=ysize

	self.id = widget_base(parent,/column,map=0,event_pro='PMI__DISPLAY__REGIONSELECTBYRANGE__EVENT')

	;BUILD INTERFACE

	controls_ysize=95
	id = widget_base(self.id,/row,/base_align_center)
	self.Navigate = Obj_new('PMI__Module__4DNavigator',id,xsize=150,ysize=controls_ysize)
	self.Lists = Obj_new('PMI__Module__ViewDataLists',id,ysize=controls_ysize)
	self.Colors = Obj_new('PMI__Module__ColorSettings',id,ysize=controls_ysize)
	self.Range = Obj_new('PMI__Module__SliderRange',id,xsize=150,ysize=controls_ysize)

	v = ['All slices','This slice','Close']
	Base = widget_base(id,/column,/frame,/base_align_center,event_pro='PMI__DISPLAY__REGIONSELECTBYRANGE__BTTNEVENT')
	for i=0,2 do id = widget_button(Base, xsize=50, ysize=28, value=v[i], uname=v[i])

	self.Message = Obj_new('PMI__Module__MessageBar',self.id)
	id = widget_base(self.id,/row,/base_align_center)
	self.Draw = Obj_new('PMI__Module__StandardDraw',id, CursorPos=CursorPos)
	self.Plot = Obj_new('PMI__Module__RoiPlot',id)

	self->Set, xsize=xsize,ysize=ysize

	;INITIALIZE

	self.Lists -> SET, /INIT, /REFRESH
	self.Lists -> GET, STDY=st, SERIES=se, REGION=re
	self.Draw -> SET, STDY=st, SERIES=se, REGION=re, /LOADDATA, /REFRESH, MODE='CLEAR', TOOL='ZoomIn'
	self.Plot -> SET, Title='Histogram', Xtitle='Pixel Value', Ytitle='Frequency', Psym=10, /REFRESH
	self.Navigate -> SET, MASTER=self.Draw, /REFRESH
	self.Range -> SET, MASTER=self.Draw, /REFRESH, /PIXELS
	self.Colors -> SET, MASTER=self.Draw, /REFRESH
	self -> SET, /DEFAULTMSG, /HISTOGRAM

	widget_control, self.id, set_uvalue = self, /map
	return, 1
END


PRO PMI__DISPLAY__REGIONSELECTBYRANGE__Define

	PMI__Module__4DNavigator__Define
	PMI__Module__SliderRange__Define
	PMI__Module__ColorSettings__Define
	PMI__Module__ViewDataLists__Define
	PMI__Module__StandardDraw__Define
	PMI__Module__RoiPlot__Define
	PMI__Module__MessageBar__Define

	struct = {PMI__DISPLAY__REGIONSELECTBYRANGE,$
		id			:0L,$
		Draw		:obj_new(),$
		Plot		:obj_new(),$
		Navigate	:obj_new(),$
		Range		:obj_new(),$
	 	Lists		:obj_new(),$
		Colors		:obj_new(),$
		Message		:obj_new()}
END