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


pro PMI__Button__Event__SeriesEditTimeResolution, ev

    Pmi__info, ev.top, Series=Series

	nt = Series->d(3)
	t0 = Series->t(0)

	if nt eq 1 then begin

    	in = cw_inputform(/pos, ev=ev $
       	, 	Title = 'Edit time resolution' $
    	,  	Labels = ['Acquisition time (sec)'] $
    	,  	DataDefaults = {t0:t0} $
    	) & if size(in,/type) eq 1 then return

    	Series->t, [in.data.t0]

	endif else begin

		t1 = Series->t(1)

    	in = cw_inputform(/pos, ev=ev $
       	,	Title = 'Edit time resolution' $
    	,  	Labels = ['Start of measurement (sec)','Temporal resolution (sec)'] $
    	,  	DataDefaults = {t0:t0, dt:t1-t0} $
    	) & if size(in,/type) eq 1 then return

    	Series->t, in.data.t0+in.data.dt*dindgen(nt)
	endelse

	PMI__control, ev.top, /refresh

end


pro PMI__Button__Control__SeriesEditTimeResolution, id, v

	PMI__Info, tlb(id), Series=Series
	widget_control, id, sensitive = obj_valid(Series)
end
function PMI__Button__SeriesEditTimeResolution, $
	parent, $
	separator=separator, $
	value = value

	if n_elements(value) eq 0 then value='Edit time resolution'

    return, widget_button(parent   $
    ,  	value  = value $
    ,  	event_pro   = 'PMI__Button__Event__SeriesEditTimeResolution'  $
    ,	pro_set_value 	= 'PMI__Button__Control__SeriesEditTimeResolution' $
    ,  	separator    = separator  )

end
