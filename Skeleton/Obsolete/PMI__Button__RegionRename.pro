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


pro PMI__Button__Event__RegionRename, ev

	PMI__info, ev.top, Stdy=S, Region=R
	in = PMI__Form(ev.top, Title='Rename region..', [$
		ptr_new({Type:'VALUE', Label:'New name?', Value:R->name(), xsize:20 })])
	IF in.cancel THEN return

  	R->Name, str_in_list(S->names(1),in.(0))
  	S->saved, 0
	PMI__control, ev.top, /refresh
end


pro PMI__Button__Control__RegionRename, id, v

	PMI__Info, tlb(id), Region=Region
	widget_control, id, sensitive = obj_valid(Region)
end
function PMI__Button__RegionRename, parent, separator=Separator

	return, widget_button(parent 				$
	, 	separator	= separator				$
	, 	value 		= 'Rename'				$
	, 	event_pro	= 'PMI__Button__Event__RegionRename'	$
	,	pro_set_value = 'PMI__Button__Control__RegionRename' $
	)


end