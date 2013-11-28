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


pro cw_editdicomheader_event, ev
end

pro cw_editdicomheader_cancel, ev
	widget_control, ev.top, get_uvalue = dummy
	widget_control, dummy, set_uvalue = 0B
	widget_control, ev.top, /destroy
end

pro cw_editdicomheader_ok, ev

	fields = widget_info(ev.top,/child)
	widget_control, fields, get_uvalue=f

	n = n_elements(f.id)
	for i=0L,n-1 do begin
		widget_control, f.id[i], get_value=text, get_uvalue=ind
		de = f.Hdr->De(ind)
		if de->vr() ne 'SQ' then de -> TextValue, text
	endfor

	widget_control, ev.top, get_uvalue = dummy
	widget_control, dummy, set_uvalue = 1B
	widget_control, ev.top, /destroy
end



function cw_editdicomheader, Hdr, parent=parent

	n = Hdr->n_de()
	if n eq 0 then begin
		ok= dialog_message(/information,'Empty DICOM header')
		return,0B
	endif

	;DEFINE DATA ELEMENT NAMES IF NOT DEFINED

	if (Hdr->de(0))->name() eq 'Unknown data element' then begin
		Template = LMU__DicomTemplate()
		for i=0L,n-1 do begin
			de = Hdr->de(i)
			tmp = Template->de(de->gr(),de->el())
			if obj_valid(tmp) then de->name,tmp->name()
		endfor
	endif

	;SELECT ONLY KNOWN DATA ELEMENTS

	Known = make_array(n,value=1B)
	for i=0L,n-1 do begin
		de = Hdr->de(i)
		name = de->name()
		if name eq 'Unknown data element' then Known[i]=0B
		if (de->gr() eq '7FE0'x) and (de->el() eq '0010'x) then Known[i]=0B
	endfor
	Ind = Where(Known eq 1B,n)
	if n eq 0 then begin
		ok= dialog_message(/information,'No known data elements')
		return,0B
	endif


	;BASE WIDGETS

	dummy = widget_base(xoffset=100, yoffset=100)

	base = widget_base(	$
		tlb_frame_attr 	= 8		$
	,	group_leader 	= dummy	$
	,	uvalue			= dummy	$
	,	modal			= 1		$
	,	column			= 1		$
	,	title 			= 'Edit DICOM header'	$
	,	TAB_MODE		= 1)


	;CREATE FIELDS

	fields 	= widget_base(base,/column,/scroll, x_scroll_size=675,y_scroll_size=500)
	id 		= lonarr(n)

	for i=0L,n-1 do begin
		de = Hdr->de(Ind[i])
		gr = change_base(de->gr(),16)
		el = change_base(de->el(),16)
		tag = '(' + strnr(gr,4) + ',' + strnr(el,4) + ')	'
		id[i] = cw_myfield(fields $
		, 	title		= tag + de->name() $
		, 	xsize		= 60 $
		, 	xlabelsize	= 275 $
		,	uvalue = Ind[i] )
		widget_control, id[i], set_value=de->TextValue()
	endfor

	widget_control, fields, set_uvalue = {id:id,Hdr:Hdr}

	;Create Buttons

	buttons = widget_base(base,/row)

	xsize = 100
	ysize = 20

	OK = widget_button(buttons				$
		, value 	= 'OK'					$
		, event_pro = 'cw_editdicomheader_ok'		$
		, xsize 	= xsize 				$
		, ysize 	= ysize					)

	CA = widget_button(buttons			$
		, value 	= 'CANCEL'				$
		, event_pro = 'cw_editdicomheader_cancel'	$
		, xsize 	= xsize					$
		, ysize 	= ysize					)

	;Realize and register

	if n_elements(parent) ne 0 then Centerinput, base, widget_info(parent,/geometry)

	widget_control, base, /realize
	xmanager, 'cw_editdicomheader', base
	widget_control, dummy, get_uvalue = uv, /destroy
	return, uv
end