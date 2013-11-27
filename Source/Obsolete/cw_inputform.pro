;Center Inputform in the PMI Display

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

PRO Centerinput,widget, pmi_pos

xCenter =	pmi_pos.XOFFSET+pmi_pos.XSIZE/2
yCenter = 	pmi_pos.YOFFSET+pmi_pos.YSIZE/2

geom = Widget_Info(widget, /Geometry)
xHalfSize = geom.Scr_XSize / 2
yHalfSize = geom.Scr_YSize / 2

Widget_Control, widget, XOffset = geom.xoffset+xCenter-xHalfSize, $
   YOffset = geom.Yoffset+yCenter-yHalfSize

END

pro cw_inputform_event, ev
end

pro cw_inputform_cancel, ev
	widget_control, ev.top, get_uvalue = dummy
	widget_control, dummy, set_uvalue = 0B
	widget_control, ev.top, /destroy
end

pro cw_inputform_ok, ev

	fields = widget_info(ev.top,/child)
	widget_control, fields, get_uvalue=f

	if f.nDroplist gt 0 then begin
		select = lonarr(f.nDroplist)
		for i=0L,f.nDroplist-1 do begin
			widget_control, f.id[i], get_value=j
			select[i] = j
		endfor
	endif else select = 0B

	nData = n_tags(f.Data)
	for i=0L,nData-1 do begin
		j = i + f.nDroplist
		widget_control, f.id[j], get_value = text
		f.Data.(i) = text
	endfor

	if f.nMultiList gt 0 then begin
		j = f.nDroplist + nData
		indices = widget_info(f.id[j],/list_select)
	endif else indices = 0B

	ReturnValue = {select:select,data:f.Data,indices:indices}

	widget_control, ev.top, get_uvalue = dummy
	widget_control, dummy, set_uvalue = ReturnValue
	widget_control, ev.top, /destroy
end


pro cw_inputform__ListSettings,nDroplist,ListNames,ListNumbers,ListDefaults

	nNames = n_elements(ListNames)
	if nNames eq 0 then begin
		nDroplist 		= 0
		ListDefaults 	= [-1]
		return
	endif

	nNum = n_elements(ListNumbers)
	if nNum eq 0 then begin
		nDroplist 	= 1
		ListNumbers = [nNames]
	endif else nDroplist = nNum

	nDef = n_elements(ListDefaults)
	if nDef eq 0 then ListDefaults = lonarr(nDroplist)
end

pro cw_inputform__DataSettings, nData, DataDefaults

	nData = n_tags(DataDefaults)
	if nData eq 0 then DataDefaults = 0B
end
pro cw_inputform__MultiSettings, nMultiList, MultiList, MultiDefault

	nMultiList = n_elements(MultiList) gt 0
	nDef = n_elements(MultiDefault)
	if nDef eq 0 then MultiDefault=0
end

function cw_inputform	$
, title 		= title 		$
, labels 		= labels 		$
, ListNames 	= ListNames 	$
, ListNumbers 	= ListNumbers 	$
, ListDefaults 	= ListDefaults 	$
, DataDefaults 	= DataDefaults	$
, MultiList		= MultiList		$
, MultiDefault 	= MultiDefault	$
, nocancel=nocancel			$
, pos = pos $
, ev = ev

	if n_elements(title) eq 0 then title = 'Please select the appropriate input'

	cw_inputform__ListSettings, nDroplist,ListNames,ListNumbers,ListDefaults
	cw_inputform__DataSettings, nData, DataDefaults
	cw_inputform__MultiSettings, nMultiList, MultiList, MultiDefault

	dummy = widget_base(xoffset 		= 200	,$
						yoffset			= 200	)

	base = widget_base(	tlb_frame_attr 	= 8		,$
						group_leader 	= dummy	,$
						uvalue			= dummy	,$
						modal			= 1		,$
						column			= 1		,$
						title 			= title	)

	;Create Fields

	fields 	= widget_base(base,/column)
	id 		= lonarr(nDroplist + nData + nMultiList)

	xlabelsize 	= 250
	xsize 		= 20

	j=0
	for i=0L,nDroplist-1 do begin
		value = ListNames[j:j + ListNumbers[i]-1]
		id[i] = cw_droplistfield(fields		$
			, title 		= labels[i] 	$
			, value 		= value 		$
			, xlabelsize 	= xlabelsize	)
		widget_control, id[i], set_value = ListDefaults[i]
		j = j + ListNumbers[i]
	endfor

	for i=0L,nData-1 do begin
		value 	= strcompress(DataDefaults.(i),/remove_all)
		j 		= i + nDroplist
		id[j] 	= cw_myfield(fields			$
			, title			= labels[j]		$
			, xsize			= xsize			$
			, xlabelsize	= xlabelsize	)
		widget_control, id[j], set_value = value
	endfor

	if nMultiList gt 0 then begin
		i 		= nDroplist + nData
		lb 		= widget_base(fields,/row)
		lab 	= widget_label(lb		$
			, xsize		= xlabelsize	$
			, value		= labels[i]		)
		id[i] 	= widget_list(lb 		$
			, multiple 	= 1 			$
			, ysize		= 5				$
			, value 	= MultiList		)
		widget_control, id[i], set_list_select=MultiDefault
	endif

	uv = {id:id,nDroplist:nDroplist,Data:DataDefaults,nMultiList:nMultiList}
	widget_control, fields, set_uvalue = uv

	;Create Buttons

	buttons = widget_base(base,/row)

	xsize = 100
	ysize = 20

	OK = widget_button(buttons				$
		, value 	= 'OK'					$
		, event_pro = 'cw_inputform_ok'		$
		, xsize 	= xsize 				$
		, ysize 	= ysize					)
	if not keyword_Set(nocancel) then $
		CA = widget_button(buttons			$
		, value 	= 'CANCEL'				$
		, event_pro = 'cw_inputform_cancel'	$
		, xsize 	= xsize					$
		, ysize 	= ysize					)

	;Realize and register
	;center widget
    if n_elements(ev) then begin
	pmi_pos = widget_info(tlb( ev.top),/geometry)
	Centerinput, base, pmi_pos
	endif

	widget_control, base, /realize
	xmanager, 'cw_inputform', base
	widget_control, dummy, get_uvalue = uv, /destroy
	return, uv
end