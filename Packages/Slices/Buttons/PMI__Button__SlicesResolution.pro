;
;
;    Copyright (C) 2009 Steven Sourbron
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
;    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
;
;
;
pro PMI__Button__Event__SlicesResolution, ev

	PMI__info, ev.top, Status=Status, Stdy=Stdy, Series=Series

	v = PMI__Form(ev.top, Title='Change matrix size', [$
		ptr_new({Type:'DROPLIST', Tag:'interp', Label:'Interpolate?', Value:['No','Yes'], Select:1}),$
		ptr_new({Type:'VALUE', Tag:'factor', Label:'Change resolution by factor', Value:2E}) ])
		IF v.cancel THEN return

	d = Series -> d()
	d[0:1] = floor(v.factor*d[0:1])

    Result = Stdy -> New('SERIES', $
        Default = Series, $
        Name = Series->name()+'['+strcompress(d[0],/remove_all)+','+strcompress(d[1],/remove_all)+']' )
    Result->m, d[0:1]

    for k=0L,d[2]*d[3]-1 do begin

       PMI__Message, status, 'Resizing', k/(d[2]*d[3]-1E)
       im = Series -> Read(Stdy->DataPath(),k)
       im = congrid(im,d[0],d[1],interp=v.interp)
       Result -> Write, Stdy->DataPath(), im, k
    endfor

    PMI__control, ev.top, /refresh
end


PRO PMI__Button__Control__SlicesResolution, id, v

	PMI__Info, tlb(id), Series=Series
    widget_control, id, sensitive=obj_valid(Series)
END

function PMI__Button__SlicesResolution,$
 	parent,$
 	separator=separator,$
 	value=value

	if n_elements(value) eq 0 then value='Change matrix size (isotropic)'

    return, widget_button(parent, $
      	separator = separator, $
      	value = value, $
      	event_pro = 'PMI__Button__Event__SlicesResolution',$
      	pro_set_value =  'PMI__Button__Control__SlicesResolution')
end
