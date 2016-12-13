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


function cw_myfield_get_value, id
	label = widget_info(id,/child)
	text = widget_info(label,/sibling)
	widget_control, text, get_value=val
	return, val
end

pro cw_myfield_set_value, id, val
	label = widget_info(id,/child)
	text = widget_info(label,/sibling)
	widget_control, text, set_value=val
end

function cw_myfield_event, ev
	ev.id = widget_info(ev.id,/parent)
	return, ev
end

function cw_myfield, parent, $
	title 		= title, $
	value		= value, $
	xlabelsize	= xlabelsize, $
	xsize		= xsize, $
	editable 	= editable, $
	uvalue 		= uvalue,$
	uname		= uname

	if n_elements(xlabelsize) eq 0 then xlabelsize=20
	if n_elements(xsize) eq 0 then xsize=20
	if n_elements(value) eq 0 then value=''
	if n_elements(title) eq 0 then title='DATA ENTRY'
	if n_elements(editable) eq 0 then editable=1

	base = widget_base(parent, /row, $
		event_func = 'cw_myfield_event', $
		pro_set_value = 'cw_myfield_set_value', $
		func_get_value = 'cw_myfield_get_value', $
		uvalue = uvalue, $
		uname = uname )

	label = widget_label(base, $
		xsize = xlabelsize, $
		value = title)

	text = widget_text(base, editable=editable, /all_events, $
		xsize = xsize, $
		value = strcompress(value,/remove_all))

	return, base
end