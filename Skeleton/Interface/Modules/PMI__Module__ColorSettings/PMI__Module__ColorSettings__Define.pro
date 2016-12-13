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


PRO PMI__Module__ColorSettings__SeriesClrs, Data=master
	tvlct, R,G,B, /get
	loadct, 0
	master -> SetSeriesClr, R,G,B
END
PRO PMI__Module__ColorSettings__RegionClrs, Data=master
	tvlct, R,G,B, /get
	loadct, 0
	master -> SetRegionClr, R,G,B
END
PRO PMI__Module__ColorSettings::ClrEvent, ev, i
	case i of
	0: xloadct,/modal,group=ev.top,updatecbdata=self.master,updatecallback='PMI__Module__ColorSettings__SeriesClrs'
	1: xloadct,/modal,group=ev.top,updatecbdata=self.master,updatecallback='PMI__Module__ColorSettings__RegionClrs'
	endcase
END
PRO PMI__Module__ColorSettings::TrimEvent, ev, i
	widget_control, ev.id, get_value = value
	Self.master -> SetSeriesTrim, value, i
END
PRO PMI__Module__ColorSettings__Event, ev
	widget_control, ev.handler, get_uvalue=self
	case widget_info(ev.id,/uname) of
		'TrimLower':self->TrimEvent, ev, 0
		'TrimUpper':self->TrimEvent, ev, 1
		'SeriesColors':self->ClrEvent, ev, 0
		'RegionColors':self->ClrEvent, ev, 1
	endcase
END


PRO PMI__Module__ColorSettings::Set $
, Refresh = Refresh $
, MASTER = master $
, ysize = ysize

	if n_elements(ysize) ne 0 then widget_control, self.id, ysize=ysize
	if n_elements(master) ne 0 then self.master=master

	if keyword_set(Refresh) then begin
		Self.master -> Get, Series=Series
		sensitive = obj_valid(Series)
		if sensitive then Trim=Series->Trim() else Trim=[0,1]
		widget_control, self.id, sensitive=sensitive
		Trim = strcompress(trim,/remove_all)
		widget_control, widget_info(self.id,find_by_uname='TrimLower'), set_value = Trim[0]
		widget_control, widget_info(self.id,find_by_uname='TrimUpper'), set_value = Trim[1]
	endif
END



PRO PMI__Module__ColorSettings::Cleanup
END
FUNCTION PMI__Module__ColorSettings::Init, parent,ysize=ysize

	BttnSize = 20

	self.id = widget_base(parent,event_pro='PMI__Module__ColorSettings__Event',/column,/base_align_center,map=0,/frame,ysize=ysize)

	id = widget_base(self.id,/row,/base_align_center)
	Sid = widget_button(id,xsize=BttnSize,ysize=BttnSize,/bitmap,uname='SeriesColors',value='colors.bmp', tooltip='Change series colormap')
	Sid = widget_button(id,xsize=BttnSize,ysize=BttnSize,/bitmap,uname='RegionColors',value='roicolors.bmp', tooltip='Change region colormap')
	id = widget_base(self.id,/column,/base_align_center)
	Sid = widget_text(id,/all_events,/editable,uname='TrimUpper',xsize=8)
	Sid = widget_text(id,/all_events,/editable,uname='TrimLower',xsize=8)

	widget_control, self.id, set_uvalue = self, /map

	return, 1
END

PRO PMI__Module__ColorSettings__Define

	struct = {PMI__Module__ColorSettings 	$
	,	id		:0L $
	,	master	:obj_new() $
	}
END