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




pro PMI__Button__Event__About, ev

	Display = PMI__DisplayNew(ev.top,'PMI__DISPLAY__WELCOME')
	PMI__Control, ev.top, /refresh
end
pro PMI__Button__Control__About, id, v
	PMI__Info, tlb(id), Viewer=Viewer, Stdy=Stdy
	widget_control, id, sensitive = (Viewer ne 'PMI__DISPLAY__WELCOME') AND obj_valid(Stdy)
end

function PMI__Button__About, parent, separator=separator

	PMI__DISPLAY__Welcome__Define

	return, widget_button(parent $
	, 	value ='About PMI' $
	, 	separator=separator $
	, 	event_pro = 'PMI__Button__Event__About' $
	,	pro_set_value = 'PMI__Button__Control__About' $
	)

end