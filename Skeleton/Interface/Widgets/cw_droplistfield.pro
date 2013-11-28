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


function cw_droplistfield_get_value, id
	label = widget_info(id,/child)
	droplist = widget_info(label,/sibling)
	return, widget_info(droplist,/droplist_select)
end

pro cw_droplistfield_set_value, id, val
	label = widget_info(id,/child)
	droplist = widget_info(label,/sibling)
	widget_control, droplist, set_droplist_select=val
end

function cw_droplistfield_event, ev
	ev.id = widget_info(ev.id,/parent)
	return, ev
end

function cw_droplistfield, parent, $
	title 		= title, $
	value		= value, $
	select		= select, $
	xlabelsize	= xlabelsize, $
	xsize		= xsize

	if n_elements(title) eq 0 then title='SELECT ITEM'
	if n_elements(value) eq 0 then value='<empty>'
	if n_elements(select) eq 0 then select=0
	if n_elements(xlabelsize) eq 0 then xlabelsize=20

	id = widget_base(parent, /row, $
		event_func 		= 'cw_droplistfield_event', $
		pro_set_value 	= 'cw_droplistfield_set_value', $
		func_get_value 	= 'cw_droplistfield_get_value')

	Sid = widget_label(id, $
		xsize = xlabelsize, $
		value = title)

	Sid = widget_droplist(id, $
		xsize = xsize, $
		value = value)
	widget_control, Sid, set_droplist_select=select

	return, id
end