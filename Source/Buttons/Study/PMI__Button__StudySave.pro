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

pro PMI__Button__Event__StudySave, ev

	PMI__info, ev.top, Stdy=s
	s -> SaveStdy
	s -> UpdateDir
end

pro PMI__Button__Control__StudySave, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	widget_control, id, sensitive=obj_valid(Stdy)
end


function PMI__Button__StudySave, parent

	return, widget_button(parent $
	, 	value='Save'	$
	, 	event_pro= 'PMI__Button__Event__StudySave' $
	, 	pro_set_value = 'PMI__Button__Control__StudySave' $
	, 	ACCELERATOR='Ctrl+S' $
	)
end