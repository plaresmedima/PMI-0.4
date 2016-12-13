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


pro PMI__Button__Event__SeriesMergeSlices, ev

	PMI__info, ev.top, Status=Status, Stdy=Stdy

	in = PMI__Form(ev.top, Title='Merge Slices..', [$
		ptr_new({Type:'LIST', Tag:'ind', Label:'Slices to merge', select:Stdy->sel(0), Value:Stdy->names(0) })])
	IF in.cancel THEN return

	n = n_elements(in.ind)
	if n eq 1 then return

	Series = objarr(n)
	d = lonarr(n,2)
	z = dblarr(n)
	t = dblarr(n)
	for k=0L,n-1 do begin
		Series[k] = Stdy -> obj(0,in.ind[k])
		IF (Series[k] -> d(2))*(Series[k] -> d(3)) GT 1 THEN BEGIN
			ok = widget_message(/information,'This function only merges series of single slices')
			return
		ENDIF
		z[k] = Series[k]->z()
		t[k] = Series[k]->t()
		d[k,*] = [Series[k]->d(0), Series[k]->d(1)]
	endfor

	s = PMI__Dicom__Sort(lindgen(n), z, t)

	dm = [max(d[*,0]),max(d[*,1]),n_elements(z),n_elements(t)]
	New = Stdy -> New('SERIES',	Name='Merged series', Default=Series[0], Domain={z:z, t:t, m:dm[0:1]})

	x = (dm[0] - d[*,0])/2
	y = (dm[1] - d[*,1])/2

	im = fltarr(dm[0],dm[1])
	for k=0L,n-1 do begin
		PMI__Message, status, 'Merging', k/(n-1E)
		l=s[k]
		im[x[l]:x[l]+d[l,0]-1,y[l]:y[l]+d[l,1]-1] = Series[l] -> Read(Stdy->DataPath())
		New -> Write, Stdy->DataPath(), im, k
		im = im*0
	endfor

	PMI__control, ev.top, /refresh
end



pro PMI__Button__Control__SeriesMergeSlices, id, v

	sensitive=0
	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then sensitive = Stdy->n(0) ge 2
    widget_control, id, sensitive=sensitive
end


function PMI__Button__SeriesMergeSlices, parent, separator=separator

	return, widget_button(parent, separator	= separator, $
	 	value = 'Merge slices..', $
		event_pro = 'PMI__Button__Event__SeriesMergeSlices', $
    	pro_set_value = 'PMI__Button__Control__SeriesMergeSlices' )
end
