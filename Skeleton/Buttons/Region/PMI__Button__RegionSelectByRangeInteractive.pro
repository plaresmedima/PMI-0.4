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


pro PMI__Button__Event__RegionSelectByRangeInteractive, ev

	PMI__info, ev.top, Stdy=Stdy, Series=S

 	R = Stdy -> New('REGION' $
	, 	name 	= 'ROI_ByRange' $
	, 	Domain 	= S->dom() $
	,	Color 	= S->Clr(SAT='R'))

	Display = PMI__DisplayNew(ev.top,'PMI__Display__RegionSelectByRange')

	Menu = widget_info(widget_info(ev.top,/child),/all_children)
	for i=0L,n_elements(Menu)-1 do widget_control, Menu[i], sensitive=0
end

pro PMI__Button__Control__RegionSelectByRangeInteractive, id, v

	PMI__Info, tlb(id), Series=Series
	widget_control, id, sensitive = obj_valid(Series)
end

function PMI__Button__RegionSelectByRangeInteractive, parent, value=value, separator=separator

	if n_elements(value) eq 0 then value='Select by Range'

	PMI__Display__RegionSelectByRange__Define

	return, widget_button(parent, $
	 	value = value, $
	 	separator = separator, $
		event_pro	= 'PMI__Button__Event__RegionSelectByRangeInteractive',	$
		pro_set_value = 'PMI__Button__Control__RegionSelectByRangeInteractive' )
end