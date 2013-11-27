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


PRO PMI__Module__CursorTools::Event, ev

	uname = widget_info(ev.id,/uname)
	IF total(['OR','AND','AND NOT','NEW'] EQ uname) EQ 0 $
	THEN BEGIN
		widget_control, ev.id, get_uvalue=uv
		device, cursor_standard=uv.crsr
		self.master -> SET, TOOL=uname
	ENDIF ELSE self.master -> SET, MODE=uname
	self->SET, /Refresh
END

PRO PMI__Module__CursorTools__Event, ev
	widget_control, ev.handler, get_uvalue=self
	self -> Event, ev
END



PRO PMI__Module__CursorTools::Get, TOOL=tool, MODE=mode, SET_BUTTON=set_button

	if arg_present(set_button) then begin
		set_button = widget_info(set_button,/child)
		set = widget_info(set_button,/BUTTON_SET)
		WHILE set EQ 0 DO BEGIN
			set_button = widget_info(set_button,/SIBLING)
			set = widget_info(set_button,/BUTTON_SET)
		ENDWHILE
	endif

	if arg_present(tool) then begin
		id=widget_info(self.id,/child)
		if not widget_info(id,/VALID_ID) then tool='ZoomIn' $
		ELSE BEGIN
			self->GET, SET_BUTTON=id
			tool = widget_info(id,/uname)
		ENDELSE
	endif

	if arg_present(mode) then begin
		id=widget_info(self.id,/child)
		id=widget_info(id,/SIBLING)
		if not widget_info(id,/VALID_ID) then mode='NEW' $
		ELSE BEGIN
			self->GET, SET_BUTTON=id
			mode = widget_info(id,/uname)
		ENDELSE
	endif
END

PRO PMI__Module__CursorTools::Set, $
	MASTER=master, $
	REFRESH=refresh

	if n_elements(master) ne 0 then begin
		self.master = master
		self->GET, TOOL=tool, MODE=mode
		master->SET, TOOL=tool, MODE=mode
	endif

	if n_elements(refresh) ne 0 then begin
		self.master->Get, Series=Series, Region=Region
		widget_control, self.id, sensitive = obj_valid(Series)
		;draw tools only if dimensions are compatible
		;make sure a sensitive tool is set
		RegionValid = obj_valid(Region)
		self->GET, TOOL=tool
		DrawTool = total(tool EQ ['RegionDisplay','RegionPolygon','RegionCircle','RegionRectangle','RegionPixel']) GT 0
		id = widget_info(self.id,find_by_uname='ShiftRegion') 	& if widget_info(id,/valid_id) then widget_control, id, sensitive=RegionValid
	;	id = widget_info(self.id,find_by_uname='RotateRegion') 	& if widget_info(id,/valid_id) then widget_control, id, sensitive=RegionValid
		id = widget_info(self.id,find_by_uname='NEW') 			& if widget_info(id,/valid_id) then widget_control, id, sensitive=DrawTool
		id = widget_info(self.id,find_by_uname='OR') 			& if widget_info(id,/valid_id) then widget_control, id, sensitive=RegionValid AND DrawTool
		id = widget_info(self.id,find_by_uname='AND') 			& if widget_info(id,/valid_id) then widget_control, id, sensitive=RegionValid AND DrawTool
		id = widget_info(self.id,find_by_uname='AND NOT') 		& if widget_info(id,/valid_id) then widget_control, id, sensitive=RegionValid AND DrawTool
		if not RegionValid then begin
			widget_control, widget_info(self.id,find_by_uname='NEW'), /set_button
			self.master->SET, MODE='NEW'
		endif
	endif

END




PRO PMI__Module__CursorTools::Cleanup
END
FUNCTION PMI__Module__CursorTools::Init, parent, ysize=ysize, all=all, scroll=scroll, zoomin=zoomin, measure=measure, region=region

	s = 20

	self.id = widget_base(parent,/row,/frame,ysize=ysize,event_pro='PMI__Module__CursorTools__Event')

	CursorTools = widget_base(self.id,row=4,/exclusive)

   	if keyword_set(all) or keyword_set(zoomin) 	then id = widget_button(CursorTools,/no_release,xsize=s,ysize=s,/bitmap,uname='ZoomIn'	,uvalue={crsr:32649},value='zoom_in.bmp',tooltip='Zoom (Rectangle)')
	if keyword_set(all) or keyword_set(scroll) 	then id = widget_button(CursorTools,/no_release,xsize=s,ysize=s,/bitmap,uname='Scroll_z',uvalue={crsr:32512},value='FlipZ.bmp', tooltip='Navigate (slice)')
	if keyword_set(all) or keyword_set(scroll) 	then id = widget_button(CursorTools,/no_release,xsize=s,ysize=s,/bitmap,uname='Scroll_t',uvalue={crsr:32512},value='FlipT.bmp', tooltip='Navigate (time)')

  	if keyword_set(all) or keyword_set(measure) then begin
  ;  	id = widget_button(CursorTools,/no_release,xsize=s,ysize=s,/bitmap,uname='MeasureAngle'		,uvalue={crsr:32649},value='measure_angle.bmp')
  		id = widget_button(CursorTools,/no_release,xsize=s,ysize=s,/bitmap,uname='MeasureDistance'	,uvalue={crsr:32649},value='measure_distance.bmp',tooltip='Measure Distance')
  ;		id = widget_button(CursorTools,/no_release,xsize=s,ysize=s,/bitmap,uname='MeasureProfile'	,uvalue={crsr:32649},value='measure_profile.bmp')
	endif

	if keyword_set(all) or keyword_set(region) 	then begin

 		id = widget_button(CursorTools,/no_release,xsize=s,ysize=s,/bitmap,uname='ShiftRegion'		,uvalue={crsr:32646},value='region_shift.bmp',tooltip='Shift Region')
   	;	id = widget_button(CursorTools,/no_release,xsize=s,ysize=s,/bitmap,uname='RotateRegion'		,uvalue={crsr:32512},value='region_rotate.bmp',tooltip='rotate region')
 		id = widget_button(CursorTools,/no_release,xsize=s,ysize=s,/bitmap,uname='RegionDisplay'	,uvalue={crsr:32649},value='region_draw.bmp',tooltip='Freehand Region')
		id = widget_button(CursorTools,/no_release,xsize=s,ysize=s,/bitmap,uname='RegionPolygon'	,uvalue={crsr:32649},value='region_polygon.bmp',tooltip='Polygon Region')
		id = widget_button(CursorTools,/no_release,xsize=s,ysize=s,/bitmap,uname='RegionCircle'		,uvalue={crsr:32649},value='region_circle.bmp', tooltip='Circular Region')
  		id = widget_button(CursorTools,/no_release,xsize=s,ysize=s,/bitmap,uname='RegionRectangle'	,uvalue={crsr:32649},value='region_rectangle.bmp', tooltip='Rectangular Region')
		id = widget_button(CursorTools,/no_release,xsize=s,ysize=s,/bitmap,uname='RegionPixel'		,uvalue={crsr:32649},value='region_pixel.bmp',tooltip='Pixel Region')

		RoiModes = widget_base(self.id,/column,/exclusive)

		id = widget_button(RoiModes,/no_release,xsize=s,ysize=s,/bitmap,uname='NEW',value='region_new.bmp',tooltip='New region')
		id = widget_button(RoiModes,/no_release,xsize=s,ysize=s,/bitmap,uname='OR',value='region_or.bmp', tooltip='Add to Region')
   		id = widget_button(RoiModes,/no_release,xsize=s,ysize=s,/bitmap,uname='AND',value='region_and.bmp',tooltip='Select from Region')
   		id = widget_button(RoiModes,/no_release,xsize=s,ysize=s,/bitmap,uname='AND NOT',value='region_andnot.bmp',tooltip='Remove from Region')

 		widget_control, widget_info(RoiModes,/child), /set_button
   	endif

	widget_control, widget_info(CursorTools,/child), /set_button
	widget_control, self.id, set_uvalue = self
	return, 1B
END


PRO PMI__Module__CursorTools__Define

	struct = {PMI__Module__CursorTools, $
		id:0L, $
		master:obj_new() }

END
