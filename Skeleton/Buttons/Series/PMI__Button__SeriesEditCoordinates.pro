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


pro PMI__Button__Event__SeriesEditCoordinates, ev

    Pmi__info, ev.top, Series=Series

    d = Series->d()
    z = Series->z()
    t = Series->t()

    slicelabels = strarr(d[2])
    timelabels = strarr(d[3])
    slicevalues = dblarr(d[2])
    timevalues = dblarr(d[3])

    for i =0,d[2]-1 do begin
       slicelabels[i] = 'Slice'+string(i+1)
       slicevalues[i] = z[i]
    end

    for i =0,d[3]-1 do begin
       timelabels[i] = 'Time'+string(i+1)
       timevalues[i] = t[i]
    end

    alllabels = [slicelabels,timelabels]
    allvalues = [slicevalues, timevalues]

    in = cw_inputformarr(          $
       title = 'Edit Header'     $
    ,  Labels = alllabels     $
    ,  DataDefaults = allvalues $
    ,  /pos, ev = ev $
    ) & if size(in,/type) eq 1 then return

    if  (n_elements(UNIQ(in.data(0:d[2]-1), SORT(in.data(0:d[2]-1)))) ne n_elements(in.data(0:d[2]-1))) then begin
       msg = ['All slice positions have to be different!']
       ok = dialog_message(msg,/information)
       goto, return
    endif

    if  (n_elements(UNIQ(in.data(d[2]:d[2]+d[3]-1), SORT(in.data(d[2]:d[2]+d[3]-1)))) ne n_elements(in.data(d[2]:d[2]+d[3]-1))) then begin
       msg = ['All times have to be different!']
       ok = dialog_message(msg,/information)
       goto, return
    endif

    Series->z, in.data(0:d[2]-1)
    Series->t, in.data(d[2]:d[2]+d[3]-1)

    return: PMI__control, ev.top, /refresh
end

pro PMI__Button__Control__SeriesEditCoordinates, id, v

	PMI__Info, tlb(id), Series=Series
	widget_control, id, sensitive = obj_valid(Series)
end
function PMI__Button__SeriesEditCoordinates, $
	parent, $
	separator=separator, $
	value = value

	if n_elements(value) eq 0 then value='Edit coordinates'

    return, widget_button(parent  $
    ,  	value    = value  $
    ,  	event_pro   = 'PMI__Button__Event__SeriesEditCoordinates' $
    ,	pro_set_value 	= 'PMI__Button__Control__SeriesEditCoordinates' $
    ,  	separator    = separator  $
    )

end