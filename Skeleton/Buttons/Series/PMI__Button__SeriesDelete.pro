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

pro PMI__Button__Event__SeriesDelete, ev

	PMI__info, ev.top, Stdy=Stdy

	v = PMI__Form(ev.top, Title='Delete Series..', $
		[ ptr_new({Type:'LIST', Tag:'ind', Label:'Series to delete', Value:Stdy->names(0), Select:Stdy->sel(0)})] $
		) & if v.cancel then return

	Stdy -> delete, 0, v.ind
	PMI__control, ev.top, /refresh
end



pro PMI__Button__Control__SeriesDelete, id, v

	PMI__Info, tlb(id), Series=Series
	widget_control, id, sensitive = obj_valid(Series)
end
function PMI__Button__SeriesDelete, parent, separator=separator

	id = widget_button(parent $
	, 	separator	= separator	$
	, 	value 		= 'Delete' $
	, 	event_pro	= 'PMI__Button__Event__SeriesDelete' $
	,	pro_set_value 	= 'PMI__Button__Control__SeriesDelete' $
	)

	return, id

end