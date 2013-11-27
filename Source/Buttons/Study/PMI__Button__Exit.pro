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


pro PMI__Button__Event__Exit, event

	widget_control, event.top, /destroy
end

pro PMI__Button__Control__Exit, id, v
end

function PMI__Button__Exit, parent

	return, widget_button(parent $
	, 	value='Exit'	$
	, 	event_pro= 'PMI__Button__Event__Exit' $
	, 	pro_set_value = 'PMI__Button__Control__Exit' $
	,  /separator $
	, 	ACCELERATOR='Ctrl+X' $
	)
end