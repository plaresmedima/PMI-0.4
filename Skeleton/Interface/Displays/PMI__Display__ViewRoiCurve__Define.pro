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


PRO PMI__Display__Event__ViewRoiCurve, ev
END

PRO PMI__Display__ViewRoiCurve::SET, $
	PMI__REFRESH=pmi__refresh, PMI__RESIZE=pmi_resize, $
 	Refresh=Refresh, $
 	xsize=xsize,ysize=ysize, $
	Xaxis=Xaxis, Yaxis=Yaxis, $
	Title=Title, Xtitle=xtitle, Ytitle=ytitle

	if keyword_set(pmi_resize) then begin
		gpmi = widget_info(/geometry, tlb(self.id))
		self -> Set, xsize=gpmi.xsize, ysize=gpmi.ysize, /Refresh
	endif
	if n_elements(xsize) ne 0 $
	or n_elements(ysize) ne 0 then begin
		widget_control, self.id, xsize=xsize, ysize=ysize
		self.Controls -> SET, xsize=xsize
		self.Plot -> Set, xsize=xsize, ysize=ysize-135
	endif
	if n_elements(Xaxis) ne 0 $
	or n_elements(Yaxis) ne 0 then begin
		self.Plot->SET, Xaxis=Xaxis, Yaxis=Yaxis, /Initialize
		self.Controls->SET, /Values
	endif
	if n_elements(Title) ne 0 then begin
		self.Plot->SET, Title=Title
		self.Controls->SET, /Values
	endif
	if n_elements(Xtitle) ne 0 then begin
		self.Plot->SET, Xtitle=Xtitle
		self.Controls->SET, /Values
	endif
	if n_elements(Ytitle) ne 0 then begin
		self.Plot->SET, Ytitle=Ytitle
		self.Controls->SET, /Values
	endif

	if keyword_set(Refresh) then self.Plot->SET, /Refresh

END
PRO PMI__Display__ViewRoiCurve::GET, CursorPos=CursorPos

	if arg_present(CursorPos) then CursorPos=self.CursorPos
END


PRO PMI__Display__ViewRoiCurve::Cleanup

	obj_destroy, [Self.Controls, Self.Plot]
	widget_control, self.id, /destroy
END

FUNCTION PMI__Display__ViewRoiCurve::Init, parent, CursorPos, xsize=xsize,ysize=ysize

	self.CursorPos=CursorPos
	self.id = widget_base(parent,/column,map=0,event_pro='PMI__Display__Event__ViewRoiCurve')

	id = widget_base(self.id,/row,/base_align_center)
	self.Controls = Obj_new('PMI__Module__PlotControls',id)
	self.Plot = Obj_new('PMI__Module__RoiPlot',self.id)

	self.Controls->SET, Plot=self.Plot
	self->SET, xsize=xsize,ysize=ysize

	widget_control, self.id, set_uvalue = self, /map

	return, 1
END


PRO PMI__Display__ViewRoiCurve__Define

	PMI__Module__PlotControls__Define
	PMI__Module__RoiPlot__Define

	struct = {PMI__Display__ViewRoiCurve 	$
	,	id			:0L 	$
	,	CursorPos	:lonarr(4) $
	,	Controls	:obj_new() $
	,	Plot		:obj_new() $
	}
END


