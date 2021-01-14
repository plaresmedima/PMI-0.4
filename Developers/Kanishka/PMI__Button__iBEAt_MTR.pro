

FUNCTION PMI__Button__Input__iBEAt_MTR, top, series, in,moco, Win

    PMI__Info, top, Stdy=Stdy

     ; MT MERGE

    PMI__Message, status, 'Merging selected MT series: MT_OFF and MT_ON'

	in = PMI__Form(top, Title='Merge Series..', [$
	ptr_new({Type:'DROPLIST', Tag:'dim', Label:'Merge dimension', Value:'Dynamics'}), $
	ptr_new({Type:'LIST', Tag:'ind', Label:'Series to merge', select:Stdy->sel(0), Value:Stdy->names(0) })])
	IF in.cancel THEN return, 0

	n = n_elements(in.ind)
	if n eq 1 then return, 0

	Series = objarr(n)
	d = lonarr(n,4)
	s = dblarr(n)

    in.dim = 1 ; for dynamics

	for k=0L,n-1 do begin
		Series[k] = Stdy -> obj(0,in.ind[k])
		d[k,*] = Series[k] -> d()
		s[k] = min(Series[k] -> c(in.dim))
	endfor

	s = sort(s)

	;DETERMINE DOMAIN = dynamics only for MT series

	dm = [max(d[*,0]),max(d[*,1]),max(d[*,2],zmax),max(d[*,3],tmax)]
	dm[2+in.dim] = total(d[*,2+in.dim])

	c=fltarr(dm[2+in.dim])
	c0=0L
	for k=0L,n-1 do begin
		l = s[k]
		c[c0:c0+d[l,2+in.dim]-1] = Series[l] -> c(in.dim)
		c0 = c0+d[l,2+in.dim]
	endfor

    Domain = {z:Series[zmax] -> c(0), t:c, m:dm[0:1]}

	;MERGE

	New = Stdy -> New('SERIES',	Name='Merged MT series', Default=Series[0], Domain=Domain)

	x = (dm[0] - d[*,0])/2
	y = (dm[1] - d[*,1])/2

	im = fltarr(dm[0],dm[1])

	ij0=[0L,0L]
	for k=0L,n-1 do begin
		PMI__Message, status, 'Merging MT sequences', k/(n-1E)
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

	PMI__control, top, /refresh


    ; MOCOMO 3D

    DynSeries = Stdy->Names(0,DefDim=3,ind=ind,sel=sel)
	in = {ser:sel, roi:0L,no_moco:1B}
	moco = {res:16E, prec:1E}

	WHILE 1 DO BEGIN

		in = PMI__Form(top, Title='Motion correction setup', [$
		ptr_new({Type:'DROPLIST',Tag:'ser', Label:'Dynamic series', Value:DynSeries, Select:in.ser}), $
		ptr_new({Type:'DROPLIST',Tag:'roi', Label:'Region of Interest', Value:['<ENTIRE FOV>',Stdy->names(1)], Select:in.roi}), $
		ptr_new({Type:'DROPLIST',Tag:'no_moco', Label:'Perform motion correction?', Value:['Yes','No'], Select:in.no_moco}) $
        ])
        IF in.cancel THEN return, 0

        IF NOT in.no_moco THEN BEGIN
		  moco = PMI__Form(top, Title='iBEAt MTR MoCo settings', [$
		    ptr_new({Type:'VALUE'	,Tag:'res', Label:'Deformation Field Resolution (pixel sizes)', Value:moco.res}), $
		    ptr_new({Type:'VALUE'	,Tag:'prec', Label:'Deformation Field Precision (pixel sizes)', Value:moco.prec}) $
		    ])
		  IF moco.cancel THEN return, 0
		ENDIF

    	IF moco.res LE 0 THEN BEGIN
    	  moco.res = 16E
    	  msg = 'Deformation Field Resolution must be > 0'
    	  goto, jump
    	ENDIF
    	IF moco.prec LE 0 THEN BEGIN
    	  moco.prec = 1E
    	  msg = 'Deformation Field Precision must be > 0'
    	  goto, jump
    	ENDIF

    	Series = Stdy->Obj(0,ind[in.ser])
    	d = Series->d()
    	Win = {p:[0L,0L,0L], n:d[0:2]}
    	IF in.roi GT 0 THEN BEGIN
    	  Region = Stdy->Obj(1,in.roi-1)
    	  dim = Region->d()
    	  if dim[3] gt 1 then begin
    	    msg = 'Region of interest must be drawn on a static image'
    	    goto, jump
    	  endif
    	  Indices = Region->Where(Stdy->DataPath(), n=cnt)
    	  if cnt eq 0 then begin
    	    msg = 'Region is not defined on this series'
    	    goto, jump
    	  endif
    	  Pos = ARRAY_INDICES(dim[0:2], Indices, /DIMENSIONS)
    	  Win.p = [$
    	    min(Pos[0,*]),$
    	    min(Pos[1,*]),$
    	    min(Pos[2,*])]
    	  Win.n = [$
    	    1+max(Pos[0,*])-min(Pos[0,*]),$
    	    1+max(Pos[1,*])-min(Pos[1,*]),$
    	    1+max(Pos[2,*])-min(Pos[2,*])]
    	  if min(Win.n) le 1 then begin
    	    msg = 'Region of interest must cover more than 1 slice'
    	    goto, jump
    	  endif
    	ENDIF
		return, 1
        JUMP: IF 'Cancel' EQ dialog_message(msg,/information,/cancel) THEN return, 0

  	ENDWHILE
END


pro PMI__Button__Event__iBEAt_MTR, ev

	PMI__Info, ev.top, Status=Status, Stdy=Stdy
	PMI__Message, status, 'Preparing calculation..'

    IF NOT PMI__Button__Input__iBEAt_MTR(ev.top,series,in,moco,win) THEN RETURN

	PMI__Message, status, 'Preparing calculation..'

	time = Series->t() - Series->t(0)
	Source = Series->Read(Stdy->DataPath())

	;Define new image series
    IF NOT in.no_moco THEN $
    Corr = Stdy -> New('SERIES', Default = Series, Name = Series->name() + '[MoCo]' )

    MTR   	= Stdy -> New('SERIES', Default = Series, Name = Series->name()+'[MT Map (%)]' )

	MTR   	-> t, Series->t(0)

    IF NOT in.no_moco THEN $
	Corr -> Trim, Series->Trim()

	MTR -> Trim, [0,100]

    PMI__Message, status, 'Calculating..'

	Model = 'Constant'
	Independent = 0B

    tt=systime(1)

	Source = TRANSPOSE(Source, [3,0,1,2])

	IF NOT in.no_moco THEN MOCOMO, Source, Model, Independent, GRID_SIZE=moco.res, TOLERANCE=moco.prec, WINDOW=win

	Source = TRANSPOSE(Source, [1,2,3,0])

    dims = SIZE(Source, /DIMENSIONS)
    MT_Map = FLTARR(dims[0],dims[1],dims[2])
    MT_Map = 100*((Source[*,*,*,0]-Source[*,*,*,1])/Source[*,*,*,0])

    IF NOT in.no_moco THEN $
	Corr -> Write, Stdy->DataPath(), Source

    MTR  -> Write, Stdy->DataPath(), MT_Map

print,'calculation time (min): ',(systime(1)-tt)/60.

    PMI__Control, ev.top, /refresh
end


pro PMI__Button__Control__iBEAt_MTR, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		sensitive = ns gt 0
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__iBEAt_MTR, parent,value=value,separator=separator

	MoCoModel_Constant__DEFINE

    if n_elements(value) eq 0 then value = 'Dynamic motion correction'

    id = widget_button(parent $
    ,   value = value  $
    ,  	event_pro = 'PMI__Button__Event__iBEAt_MTR' $
    ,	pro_set_value = 'PMI__Button__Control__iBEAt_MTR' $
    ,  	separator = separator )

    return, id
end

