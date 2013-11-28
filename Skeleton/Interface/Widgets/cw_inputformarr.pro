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


;Center Inputform in the PMI Display
PRO Centerinput,widget, pmi_pos

xCenter =	pmi_pos.XOFFSET+pmi_pos.XSIZE/2
yCenter = 	pmi_pos.YOFFSET+pmi_pos.YSIZE/2

geom = Widget_Info(widget, /Geometry)
xHalfSize = geom.Scr_XSize / 2
yHalfSize = geom.Scr_YSize / 2

Widget_Control, widget, XOffset = geom.xoffset+xCenter-xHalfSize, $
   YOffset = geom.Yoffset+yCenter-yHalfSize

END

pro cw_inputformarr_event, ev
end

pro cw_inputformarr_cancel, ev
	widget_control, ev.top, get_uvalue = dummy
	widget_control, dummy, set_uvalue = 0B
	widget_control, ev.top, /destroy
end

pro cw_inputformarr_ok, ev

	fields = widget_info(ev.top,/child)
	widget_control, fields, get_uvalue=f

	nData = n_elements(f.Data)
	for i=0L,nData-1 do begin
		j = i
		widget_control, f.id[j], get_value = text
		f.Data[i] = text
	endfor

	ReturnValue = {data:f.Data}

	widget_control, ev.top, get_uvalue = dummy
	widget_control, dummy, set_uvalue = ReturnValue
	widget_control, ev.top, /destroy
end


pro cw_inputformarr__DataSettings, nData, DataDefaults

	nData = n_elements(DataDefaults)
	if nData eq 0 then DataDefaults = 0
end

function cw_inputformarr	$
, title 		= title 		$
, labels 		= labels 		$
, DataDefaults 	= DataDefaults	$
,	nocancel=nocancel     $
, pos = pos, ev = ev

	if n_elements(title) eq 0 then title = 'Please select the appropriate input'

	cw_inputformarr__DataSettings, nData, DataDefaults


	dummy = widget_base(xoffset 		= 100	,$
						yoffset			= 100	$
						)

	base = widget_base(	tlb_frame_attr 	= 8		,$
						group_leader 	= dummy	,$
						uvalue			= dummy	,$
						modal			= 1		,$
						column			= 1		,$
						title 			= title	,$
						TAB_MODE		= 1)

	;Create Fields

	fields 	= widget_base(base,/column,/scroll, x_scroll_size=200,y_scroll_size =500)
	id 		= lonarr(nData)


	for i=0L,nData-1 do begin
		value 	= string(DataDefaults[i])
		j 		= i
		id[j] 	= fsc_field(fields			$
			, title			= labels[j]		$
			, labelsize	= 100	$
			, xsize			= 10			$
			, /highlight	$
			, uname = strcompress(string('field_',i),/remove_all)				)
		widget_control, id[j], set_value = value
	endfor


	uv = {id:id,Data:DataDefaults}
	widget_control, fields, set_uvalue = uv

	;Create Buttons

	buttons = widget_base(base,/row)

	xsize = 100
	ysize = 20

	OK = widget_button(buttons				$
		, value 	= 'OK'					$
		, event_pro = 'cw_inputformarr_ok'		$
		, xsize 	= xsize 				$
		, ysize 	= ysize						)
	if not keyword_Set(nocancel) then $
		CA = widget_button(buttons			$
		, value 	= 'CANCEL'				$
		, event_pro = 'cw_inputformarr_cancel'	$
		, xsize 	= xsize					$
		, ysize 	= ysize					)

	;Realize and register

	if KEYWORD_SET(pos) then begin
	pmi_pos = widget_info(tlb( ev.top),/geometry)
	Centerinput, base, pmi_pos
	endif


	widget_control, base, /realize

	;highlight first text field
	firstID = widget_info(base, find_by_uname='field_0')
	if widget_info(firstID,/valid_id) then begin
		widget_control, firstID, get_value=text
    	text = text[0]
    	Widget_Control, firstID, set_text_select=[0, Strlen(text)], /input_focus
	endif
	;;;;;;;;;;;;;;;;;;;;;;;;;;;

	xmanager, 'cw_inputformarr', base
	widget_control, dummy, get_uvalue = uv, /destroy
	return, uv
end