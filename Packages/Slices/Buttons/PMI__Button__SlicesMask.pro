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

pro PMI__Button__Event__SlicesMask, ev

	PMI__info, ev.top, Status=Status, Stdy=Stdy

	Series  = Stdy->Names(0,ns)
	Regions = Stdy->Names(1,nr)

	v = PMI__Form(ev.top, Title='Mask', [$
		ptr_new({Type:'DROPLIST', Tag:'series', Label:'Mask Series', Value:Series, Select:Stdy->sel(0)}),$
		ptr_new({Type:'DROPLIST', Tag:'region', Label:'Mask ROI', Value:Regions, Select:Stdy->sel(1)}),$
		ptr_new({Type:'VALUE', Tag:'bck', Label:'Background value', Value:0E}) ])
		IF v.cancel THEN return

	Series = Stdy->Obj(0,v.series)
	Region = Stdy->Obj(1,v.region)

	d = Series -> d()
	dROI = Region -> d()
	if total(d eq dROI) ne 4 then begin
		ok = dialog_message(/information,'Mask ROI and Series must have the same dimensions')
		return
	endif

 	New = Stdy->New('SERIES',Default=Series, Name=Series->name() +'[Masked]')

	d = Series -> d()
	for k=0L,d[2]*d[3]-1 do begin

		PMI__Message, status, 'Masking', k/(d[2]*d[3]-1E)

		dat = Series -> Read(Stdy->DataPath(),k)
		ind = Region -> Where(Stdy->DataPath(),k,n=n,/inverse)
		if n gt 0 then dat[ind] = v.bck
		New -> Write, Stdy->DataPath(), dat, k
	endfor

	PMI__control, ev.top, /refresh
end


pro PMI__Button__Control__SlicesMask, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 0)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

FUNCTION PMI__Button__SlicesMask,$
 	parent,$
 	separator=separator,$
 	value=value

	if n_elements(value) eq 0 then value='Mask slices'

	RETURN, widget_button(parent, 	$
	 	value		= value, $
		event_pro	= 'PMI__Button__Event__SlicesMask', $
		pro_set_value =  'PMI__Button__Control__SlicesMask', $
		separator	= separator )

END