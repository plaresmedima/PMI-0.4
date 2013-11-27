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

pro PMI__Button__Event__SeriesMerge, ev

	PMI__info, ev.top, Status=Status, Stdy=Stdy

	in = PMI__Form(ev.top, Title='Merge Series..', [$
	ptr_new({Type:'DROPLIST', Tag:'dim', Label:'Merge which dimension?', select:1L, Value:['Slices', 'Dynamics']}), $
	ptr_new({Type:'LIST', Tag:'ind', Label:'Series to merge', select:Stdy->sel(0), Value:Stdy->names(0) })])
	IF in.cancel THEN return

	n = n_elements(in.ind)
	if n eq 1 then return

	Series = objarr(n)
	d = lonarr(n,4)
	s = dblarr(n)

	for k=0L,n-1 do begin
		Series[k] = Stdy -> obj(0,in.ind[k])
		d[k,*] = Series[k] -> d()
		s[k] = min(Series[k] -> c(in.dim))
	endfor

	s = sort(s)



	;DETERMINE DOMAIN



	dm = [max(d[*,0]),max(d[*,1]),max(d[*,2],zmax),max(d[*,3],tmax)]
	dm[2+in.dim] = total(d[*,2+in.dim])

	c=fltarr(dm[2+in.dim])
	c0=0L
	for k=0L,n-1 do begin
		l = s[k]
		c[c0:c0+d[l,2+in.dim]-1] = Series[l] -> c(in.dim)
		c0 = c0+d[l,2+in.dim]
	endfor

	case in.dim of
	1:	Domain = {z:Series[zmax] -> c(0), t:c, m:dm[0:1]}
	0:	Domain = {z:c, t:Series[tmax] -> c(1), m:dm[0:1]}
	endcase



	;MERGE DATA



	New = Stdy -> New('SERIES',	Name='Merged series', Default=Series[0], Domain=Domain)

	x = (dm[0] - d[*,0])/2
	y = (dm[1] - d[*,1])/2

	im = fltarr(dm[0],dm[1])

	ij0=[0L,0L]
	for k=0L,n-1 do begin
		PMI__Message, status, 'Merging', k/(n-1E)
		l=s[k]
		for r=0L,d[l,2]*d[l,3]-1 do begin
			im[x[l]:x[l]+d[l,0]-1,y[l]:y[l]+d[l,1]-1] = Series[l] -> Read(Stdy->DataPath(),r)
			ij = ij0 + reform_ind(d[l,2:3],ind=r)
			New -> Write, Stdy->DataPath(), im, ij[0], ij[1]
       		mins = min(im,max=maxs)
       		if k eq 0 then Trim = [mins,maxs] else begin
         		Trim[0] = min([Trim[0],mins])
         		Trim[1] = max([Trim[1],maxs])
       		endelse
			im = im*0
		endfor
		ij0[in.dim] = ij0[in.dim] + d[l,2+in.dim]
	endfor

	New -> Trim, float(Trim)

	PMI__control, ev.top, /refresh
end



pro PMI__Button__Control__SeriesMerge, id, v

	sensitive=0
	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then sensitive = Stdy->n(0) ge 2
    widget_control, id, sensitive=sensitive
end


function PMI__Button__SeriesMerge, parent, separator=separator

	id = widget_button(parent 	$
	, 	separator	= separator $
	, 	value 		= 'Merge..'	$
	,	event_pro 	= 'PMI__Button__Event__SeriesMerge'	$
    ,	pro_set_value 	= 'PMI__Button__Control__SeriesMerge' $
	)

	return, id
end
