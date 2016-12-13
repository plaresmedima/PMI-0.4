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
pro PMI__Button__Event__ExtractTimeWindow, ev

	PMI__Info, ev.top, Status=Status, Stdy=Stdy
	DynSeries = Stdy->Names(0,DefDim=3,ind=ind,sel=sel)
	in = PMI__Form(ev.top, Title='Extract time window', [$
		ptr_new({Type:'DROPLIST',Tag:'ser', Label:'Dynamic Series', Value:DynSeries, Select:sel}), $
		ptr_new({Type:'DROPLIST',Tag:'roi', Label:'Dynamic Region', Value:Stdy->names(1), Select:Stdy->sel(1)})])
	IF in.cancel THEN return

    Series = Stdy->Obj(0,ind[in.ser])
    Roi = Stdy->Obj(1,in.roi)
    New = Stdy->New('SERIES', Default=Series,  Name=Series->name()+'[Window]' )

	d = Series->d()
	slice = fltarr(d[0],d[1])

	for j=0L,d[2]-1 do begin

		PMI__Message, status, 'Calculating', j/(d[2]-1E)

		P = PMI__PixelCurve(Stdy->DataPath(),Series,Roi,z=Series->z(j),cnt=cnt,ind=ind)

		if cnt gt 0 then for k=0L,d[3]-1 do begin
			slice = slice*0
			slice[ind] = P[*,k]
			New->Write, Stdy->DataPath(), slice, j, k
		endfor
	endfor

    PMI__Control, ev.top, /refresh
end


pro PMI__Button__Control__ExtractTimeWindow, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 0)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__ExtractTimeWindow, parent,value=value,separator=separator

    if n_elements(value) eq 0 then value = 'Extract time window'

    id = widget_button(parent $
    ,   value = value  $
    ,  	event_pro = 'PMI__Button__Event__ExtractTimeWindow' $
    ,	pro_set_value = 'PMI__Button__Control__ExtractTimeWindow' $
    ,  	separator = separator )

    return, id
end

