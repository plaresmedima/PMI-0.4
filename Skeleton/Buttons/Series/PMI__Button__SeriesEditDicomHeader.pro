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


pro PMI__Button__Event__SeriesEditDicomHeader, ev

    Pmi__info, ev.top, Stdy=Stdy, Series=Series

	stdy->saved, 0B eq cw_editdicomheader(Series, parent=tlb(ev.top))

end


pro PMI__Button__Control__SeriesEditDicomHeader, id, v

	PMI__Info, tlb(id), Series=Series
	widget_control, id, sensitive = obj_valid(Series)
end
function PMI__Button__SeriesEditDicomHeader, $
	parent, $
	separator=separator, $
	value=value

	if n_elements(value) eq 0 then value='Edit header'

    id = widget_button(parent $
    ,  	value    = value  $
    ,  	event_pro   = 'PMI__Button__Event__SeriesEditDicomHeader' $
    , 	pro_set_value 	= 'PMI__Button__Control__SeriesEditDicomHeader' $
    ,  	separator    = separator          )

    return, id

end