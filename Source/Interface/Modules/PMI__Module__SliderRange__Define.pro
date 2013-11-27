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


FUNCTION PMI__Module__SliderRange::Event, ev

	return, {id:self.id, top:ev.top, handler:0L, value:ev.value}
END

FUNCTION PMI__Module__SliderRange__Event, ev
	widget_control, ev.handler, get_uvalue=self
	return, self -> Event(ev)
END

PRO PMI__Module__SliderRange::Get, $
	Range=Range

	if arg_present(Range) then begin
		self.master -> Get, Series=Series
		v = Series->Trim()
		widget_control, Self.SliderId[0], get_value=p1
		widget_control, Self.SliderId[1], get_value=p0
		Range = v[0] + (v[1]-v[0])*[p0,p1]/100E
	endif
END

PRO PMI__Module__SliderRange::Set, $
	Refresh = Refresh, $
	MASTER = master, $
	xsize = xsize, $
	ysize = ysize, $
	Pixels = Pixels

	if n_elements(xsize) ne 0 then for i=0L,1 do widget_control, Self.SliderId[i], xsize=xsize
	if n_elements(ysize) ne 0 then widget_control, self.id, ysize=ysize
	if n_elements(master) ne 0 then self.master = master
	if keyword_set(Refresh) then begin
		self.master->Get, Series=Series, Region=Region
		for i=0,1 do widget_control, Self.SliderId[i], sensitive = obj_valid(Series) and obj_valid(Region)
	endif
	if keyword_set(pixels) then begin
		self.master -> Get, Series=S, Region=R
		if obj_valid(S) and obj_valid(R) then begin
			self.master -> Get, Raw=Im
			self->GET, Range=r
 			ind = where( (Im ge r[0]) and (Im le r[1]), cnt)
 			d = size(im,/dimensions)
 			if cnt gt 0 then begin
 				bin = bytarr(d[0]*d[1])
 				bin[ind] = 1
 				self.master->SetRegion, Bin
 			endif
 		endif
	endif
END


PRO PMI__Module__SliderRange::Cleanup
END
FUNCTION PMI__Module__SliderRange::Init, parent, xsize=xsize,ysize=ysize

	self.id = widget_base(parent,event_func='PMI__Module__SliderRange__Event',/base_align_center,map=0,/column,/frame,ysize=ysize)

	id = widget_base(self.id)
	Self.SliderId[0] = widget_slider(id,Title='Ceiling (%)',/drag,xsize=xsize,maximum=100,minimum=0,value=80)
	id = widget_base(self.id)
	Self.SliderId[1] = widget_slider(id,Title='Floor (%)',/drag,xsize=xsize,maximum=100,minimum=0,value=20)

	widget_control, self.id, set_uvalue = self, /map
	return, 1B
END
PRO PMI__Module__SliderRange__Define

	struct = {PMI__Module__SliderRange,  $
		id:0L, $
		SliderId: lonarr(2), $
		master: Obj_new() $
	}

END