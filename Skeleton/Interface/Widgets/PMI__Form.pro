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


pro PMI__Form_event, ev
end

pro PMI__Form__Exit, ev

	fieldsbase = widget_info(ev.top,/child)
	widget_control, fieldsbase, get_uvalue=ptr
	widget_control, ev.id, get_value=Button

	if Button eq 'CANCEL' then begin
		ptr_free, *ptr
		*ptr = {cancel:1B}
		widget_control, ev.top, /destroy
		return
	endif

	ReturnValue = {cancel:0B}

	fields = *ptr
	nfields = n_elements(fields)

	field_id = widget_info(fieldsbase,/all_children)
	for i=nfields-1L,0L,-1L do begin
		id = widget_info(widget_info(field_id[i],/child),/sibling)
		f = (*fields[i])
		tagdefined = 1 eq total('TAG' eq tag_names(f))
		IF tagdefined THEN tag=f.tag ELSE tag='item'+strcompress(i,/remove_all)
		case f.type of
		'VALUE':begin
			widget_control, id, get_value = value
			f.value = value		;convert to the appropriate type
			ReturnValue = Create_Struct(tag, f.value, ReturnValue)
			end
		'DROPLIST':ReturnValue = Create_Struct(tag, widget_info(id,/droplist_select), ReturnValue)
		'LIST':	ReturnValue = Create_Struct(tag, widget_info(id,/list_select), ReturnValue)
		endcase

	endfor

	ptr_free, fields	;delete the input structures
	*ptr = ReturnValue
	widget_control, ev.top, /destroy
end


function PMI__Form, top, fields $
, 	TITLE=title $
,	XSIZE_MAX=xsize_max $
,	YSIZE_MAX=ysize_max $
,	XLABELSIZE=xlabelsize


	nfields = n_elements(fields)
	if nfields eq 1 then fields = [fields]
	ptr = ptr_new(fields)	;Pointer to the return value



;	DEFAULTS AND SIZES



	if n_elements(title) eq 0 then title = 'Please select the appropriate input'
	IF n_elements(xsize_max) EQ 0 THEN xsize_max=1000
	IF n_elements(ysize_max) EQ 0 THEN ysize_max=1000

	IF n_elements(xlabelsize) EQ 0 THEN BEGIN ;determine labelsize (min 120)
		xlabelsize = 120
		FOR i=0L,nfields-1 DO $
			xlabelsize=max([xlabelsize,6*strlen((*fields[i]).label)])
	ENDIF



;	BUILD FORM



	base = widget_base(group_leader=top, /modal, /column, title=title, tlb_frame_attr=8)

	fieldsbase = widget_base(base, /column,/scroll, uvalue=ptr)
	for i=0L,nfields-1 do begin
		f = *fields[i]
		field_id = widget_base(fieldsbase,/row,/frame)
		id = widget_label(field_id, value=f.label, xsize=xlabelsize)
		case f.type of
		'VALUE':begin
			if 1 eq total('XSIZE' eq tag_names(f)) then xsize=f.xsize else xsize=10
			value = f.value
			if size(f.value,/tname) eq 'BYTE' then value=1L*value
			if size(f.value,/tname) ne 'STRING' then value=strcompress(value,/remove_all)
			id = widget_text(field_id, value=value, xsize=xsize, /editable)
			end
		'DROPLIST':	begin
			id = widget_droplist(field_id, value=f.value)
			tagdefined = 1 eq total('SELECT' eq tag_names(f))
			IF tagdefined THEN select=f.select ELSE select=0
			widget_control, id, set_droplist_select=select
			end
		'LIST':	begin
		;	id = widget_list(field_id, value=f.value, ysize=n_elements(f.value), /multiple)
			id = widget_list(field_id, value=f.value, ysize=min([20,n_elements(f.value)]), /multiple)
			tagdefined = 1 eq total('SELECT' eq tag_names(f))
			IF tagdefined THEN select=f.select ELSE select=0
			widget_control, id, set_list_select=select
			end
		endcase
	endfor

	g = widget_info(fieldsbase,/geometry)
	if g.scr_xsize gt xsize_max then widget_control, fieldsbase, scr_xsize=xsize_max
	if g.scr_ysize gt ysize_max then widget_control, fieldsbase, scr_ysize=0.6*ysize_max

	xsize = 100 & ysize = 20
	buttons = widget_base(base,/row, event_pro='PMI__Form__Exit')
	id = widget_button(buttons, value='OK', xsize=xsize, ysize=ysize)
	id = widget_button(buttons, value='CANCEL', xsize=xsize, ysize=ysize)

	widget_control, base, /realize

	xmanager, 'PMI__Form', base

	ReturnValue = *ptr
	ptr_free, ptr
	return, ReturnValue
end