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


;include possibility of multiple masters

FUNCTION PMI__Module__4DNavigator::Event, ev
	i = where(ev.id eq Self.SliderId)
	self.master -> Get, CursorPos=P
	P[2+i[0]] = ev.value
	self.master -> Set, CursorPos=P, /LoadData, /Refresh
	return, {id:self.id, top:ev.top, handler:0L, dim:i[0], value:ev.value}
END
FUNCTION PMI__Module__4DNavigator__Event, ev
	widget_control, ev.handler, get_uvalue=self
	return, self -> Event(ev)
END


PRO PMI__Module__4DNavigator::Set, $
	Refresh = Refresh, $
	MASTER = master, $
	xsize = xsize, $
	ysize = ysize

	if n_elements(xsize) ne 0 then for i=0L,1 do widget_control, Self.SliderId[i], xsize=xsize
	if n_elements(ysize) ne 0 then widget_control, self.id, ysize=ysize
	if n_elements(master) ne 0 then self.master = master
	if keyword_set(Refresh) then begin
		self.master->Get, Series=Series, CursorPos=CursorPos
		if obj_valid(Series) then D=[Series->d(2),Series->d(3)] else D=[1,1]
		for i=0,1 do begin
			widget_control, Self.SliderId[i], set_slider_max = D[i]-1, sensitive = D[i] gt 1
			widget_control, Self.SliderId[i], set_value = CursorPos[2+i]
		endfor
	endif

END


PRO PMI__Module__4DNavigator::Cleanup
END
FUNCTION PMI__Module__4DNavigator::Init, parent, xsize=xsize,ysize=ysize

	self.id = widget_base(parent,event_func='PMI__Module__4DNavigator__Event',/base_align_center,map=0,/column,/frame,ysize=ysize)
	Labels = ['Slice Position','Acquisition Time']
	for i=0,1 do begin
		id = widget_base(self.id)
		Self.SliderId[i] = widget_slider(id,Title=Labels[i],/drag,xsize=xsize)
	endfor
	widget_control, self.id, set_uvalue = self, /map
	return, 1B
END
PRO PMI__Module__4DNavigator__Define

	struct = {PMI__Module__4DNavigator 	$
	,	id:0L $
	,	SliderId: lonarr(2) $
	,	master: Obj_new() $
	}

END