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


pro PMI__Button__Event__RegionShrink, ev

	PMI__info, ev.top, Stdy=Stdy, status=status

	in = PMI__Form(ev.top, Title='Shrink Region', [$
	ptr_new({Type:'DROPLIST',Tag:'roi', Label:'Region', Value:Stdy->names(1), Select:stdy->sel(1)}), $
	ptr_new({Type:'DROPLIST',Tag:'ser', Label:'Series', Value:Stdy->Names(0), Select:Stdy->sel(0)}), $
	ptr_new({Type:'DROPLIST',Tag:'type', Label:'Shrink by...', Value:['Range','Percentile'], Select:0}), $
	ptr_new({Type:'VALUE'	,Tag:'min', Label:'Minimum (%)', Value:90E}), $
	ptr_new({Type:'VALUE'	,Tag:'max', Label:'Maximum (%)', Value:100E})])
	IF in.cancel THEN return

	if in.min lt 0 then in.min=0
	if in.max gt 100 then in.max=100

    Ser = Stdy->Obj(0,in.ser)
    Roi = Stdy->Obj(1,in.roi)

	v = PMI__RoiValues(Stdy->DataPath(),Ser,Roi,Status,Cnt=n)
	if n le 1 then begin
		ok = dialog_message(/information,'ROI ' + Roi->Name() + ' is not defined on series ' + Series->Name())
		return
	endif

	if in.type eq 0 then begin
		minv = min(v)
		maxv = max(v)
		v0 = minv + (in.min/100)*(maxv-minv)
		v1 = minv + (in.max/100)*(maxv-minv)
	endif else begin
		p = percentiles(v,[in.min,in.max])
		v0 = p[0]
		v1 = p[1]
	endelse

	d = Roi->d()
	z = Roi->z()
	t = Roi->t()

	Bin = bytarr(d[0],d[1])

	Shrink = Stdy -> New('REGION', $
	    Name 	= Roi->name() + '[' + strcompress(in.min,/remove_all) + ' % < ' + strcompress(in.max,/remove_all) +' %]', $
	    domain 	= Roi->dom(), color = Roi->clr())

	for k=0L,d[2]*d[3]-1 do begin

		PMI__Message, id, 'Shrinking ' + Roi->name(), k/(d[2]*d[3]-1E)

		p = reform_ind(d[2:3],ind=k)

		v = PMI__RoiSliceValues(Stdy->DataPath(), Ser, Roi, z[p[0]], t[p[1]], ind=i, cnt=cnt)

		if cnt gt 0 then begin
			j = where((v ge v0) and (v le v1), cnt)
			if cnt gt 0 then begin
				Bin[i[j]] = 1B
				Shrink -> Write, Stdy->DataPath(), Bin, p[0], p[1]
				Bin[i[j]] = 0B
			endif
		endif

	endfor

	PMI__Control, ev.top, /refresh
end


pro PMI__Button__Control__RegionShrink, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns ge 1) and (nr ge 1)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end


function PMI__Button__RegionShrink, parent, separator=separator, value=value

	if n_elements(value) eq 0 then value  = 'Shrink'

  	id = widget_button(parent, separator = separator, value = value, $
		pro_set_value = 'PMI__Button__Control__RegionShrink', $
	 	event_pro = 'PMI__Button__Event__RegionShrink'	)

	return, id
end