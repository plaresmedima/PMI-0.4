pro PMI__Button__Event__TMUH_SeriesMerge, ev

	PMI__info, ev.top, Status=Status, Stdy=Stdy

	in = PMI__Form(ev.top, Title='Merge Series..', [$
;	ptr_new({Type:'DROPLIST', Tag:'dim', Label:'Merge which dimension?', select:1L, Value:['Slices', 'Dynamics']}), $
	ptr_new({Type:'LIST', Tag:'ind', Label:'Series to merge', select:Stdy->sel(0), Value:Stdy->names(0) })])
	IF in.cancel THEN return

	dim=1

	n = n_elements(in.ind)
	if n eq 1 then return

	Series = objarr(n)
	d = lonarr(n,4)
	s = dblarr(n)

	for k=0L,n-1 do begin
		Series[k] = Stdy -> obj(0,in.ind[k])
		d[k,*] = Series[k] -> d()
		s[k] = min(Series[k] -> c(dim))
	endfor

	s = sort(s)



	;DETERMINE DOMAIN



	dm = [max(d[*,0]),max(d[*,1]),max(d[*,2],zmax),max(d[*,3],tmax)]
	dm[2+dim] = total(d[*,2+dim])

	c=fltarr(dm[2+dim])
	c0=0L
	for k=0L,n-1 do begin
		l = s[k]
		c[c0:c0+d[l,2+dim]-1] = Series[l] -> c(dim)
		c0 = c0+d[l,2+dim]
	endfor

	case dim of
	1:	Domain = {z:Series[zmax] -> c(0), t:c, m:dm[0:1]}
	0:	Domain = {z:c, t:Series[tmax] -> c(1), m:dm[0:1]}
	endcase



	;MERGE DATA



	New = Stdy -> New('SERIES',	Name='VIBE - Dynamic', Default=Series[0], Domain=Domain)

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
		ij0[dim] = ij0[dim] + d[l,2+dim]
	endfor

	New -> Trim, float(Trim)


;CALCULATE DESCRIPTIVE INDICES


	MAX = Stdy->New('SERIES',Default=New,Name=New->Name()+'[Maximum (a.u.)]') & MAX->t, New->t(0)
	AUC = Stdy->New('SERIES',Default=New,Name=New->Name()+'[Area under the Curve (a.u.)]')	& AUC->t, New->t(0)

	MAX->Trim, 0E, 1
	AUC->Trim, 0E, 1

	;START OF CALCULATION

	time = New->t()
	time = float(time-time[0])
	d = New->d()

	MaxIm = fltarr(d[0]*d[1])
	AUCim = fltarr(d[0]*d[1])

	;LOOP OVER THE SLICES

	for i=0L,d[2]-1 do begin

		;calculate baseline P0
		P0 = New -> Read(Stdy->DataPath(),i,0)
		nozero = where(P0 NE 0, cnt_nozero)

		if cnt_nozero GT 0 then for j=0L,d[3]-1 do begin

			PMI__Message, Status, 'Calculating', (i*d[3]+j)/(d[2]*d[3]-1E)
			P = New -> Read(Stdy->DataPath(),i,j)
			P = P[nozero]-P0[nozero]

			if j eq 0 then begin
				MaxIm[nozero] = P
			endif else begin
				ind = where(P gt MaxIm[nozero], cnt)
				if cnt gt 0 then begin
					MaxIm[nozero[ind]] = P[ind]
				endif
				AUCim[nozero] = AUCim[nozero] + (Time[j]-Time[j-1])*P
			endelse
		endfor

		MAX -> Write, Stdy->DataPath(), MaxIm, i
		AUC -> Write, Stdy->DataPath(), AUCim, i

		MAX->Trim, max([MAX->Trim(1),max(MaxIm)]), 1
		AUC->Trim, max([AUC->Trim(1),max(AUCim)]), 1

		MaxIm[nozero] = 0
		AUCim[nozero] = 0
	endfor

	PMI__control, ev.top, /refresh
end

pro PMI__Button__Control__SeriesMerge, id, v

	sensitive=0
	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then sensitive = Stdy->n(0) ge 2
    widget_control, id, sensitive=sensitive
end

function PMI__Button__TMUH_SeriesMerge, parent, separator=separator, value=value

	if n_elements(value) eq 0 then value = 'Merge Series'

	id = widget_button(parent $
	, 	separator	= separator $
	, 	value 		= value	$
	,	event_pro 	= 'PMI__Button__Event__TMUH_SeriesMerge' $
    ,	pro_set_value 	= 'PMI__Button__Control__SeriesMerge' )

	return, id
end
