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



pro PMI__Button__Event__2d1dView, ev

   	PMI__Control, ev.top, Viewer = 'PMI__DISPLAY__2D1DVIEW'
   	PMI__Control, ev.top, /refresh
end
pro PMI__Button__Control__2d1dView, id, v

	PMI__Info, tlb(id), Stdy=Stdy, Viewer=Viewer
	sensitive = (Viewer ne 'PMI__DISPLAY__2D1DVIEW') AND obj_valid(Stdy)
	widget_control, id, sensitive = sensitive
end

function PMI__Button__2d1dView, parent

	PMI__DISPLAY__2d1dView__Define

	return, widget_button(parent $
	, 	value='2D + 1D' $
	, 	event_pro= 'PMI__Button__Event__2d1dView' $
	,	pro_set_value= 'PMI__Button__Control__2d1dView' $
	)



end