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
pro PMI__Button__Event__DynamicRegrid, ev

	PMI__Info, ev.top, Status=Status, Stdy=Stdy, Series=Series

	t = Series->t()
	d = Series->d()

	v = PMI__Form(ev.top, Title='Regridding dynamics', $
		[ptr_new({Type:'VALUE', Tag:'dt', Label:'New temporal resolution (sec)', Value:t[1]-t[0]})])
	IF v.cancel THEN return

	n_regr = 1 + floor((t[d[3]-1]-t[0])/v.dt)
	t_regr = t[0]+v.dt*findgen(n_regr)

	Regr = Stdy->New('SERIES', $
		Name 	= Series->name() + '[dt =' + strcompress(v.dt,/remove_all)+' sec]', $
		Color 	= Series->clr(), $
		Domain 	= {z:Series->z(), t:t_regr, m:Series->m()})

	im_regr = fltarr(d[0]*d[1],n_regr)

	FOR i=0L,d[2]-1 DO BEGIN
		PMI__Message, Status, 'Regridding ', i/(d[2]-1E)
		im = Series->Read(Stdy->DataPath(),i,-1)
		im = reform(im,d[0]*d[1],d[3],/overwrite)
		FOR k=0L,d[0]*d[1]-1 DO im_regr[k,*] = interpol(reform(im[k,*]),t,t_regr)
		Regr->Write, Stdy->DataPath(), im_regr, i,-1
	ENDFOR

	PMI__Control, ev.top, /refresh
end




pro PMI__Button__Control__DynamicRegrid, id, v

	PMI__Info, tlb(id), Series=Series
	sensitive = 0
	if obj_valid(Series) then sensitive = Series->d(3) gt 1
    widget_control, id, sensitive=sensitive
end


function PMI__Button__DynamicRegrid, parent,separator=separator

    return, widget_button(parent,$
    	separator = separator,$
    	value = 'Regrid',$
		event_pro = 'PMI__Button__Event__DynamicRegrid',$
		pro_set_value = 'PMI__Button__Control__DynamicRegrid')
end