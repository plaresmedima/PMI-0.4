

FUNCTION PMI__Button__Input__iBEAt_MTR, top, series, aif, in, moco, Win

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

    ; MOCOMO

    DynSeries = Stdy->Names(0,DefDim=3,ind=ind,sel=sel)
	in = {ser:sel, roi:0L, no_moco:1B}
	moco = {res:16E, prec:1E}


    PRINT, SIZE(DynSeries,/N_DIMENSIONS)


	WHILE 1 DO BEGIN

		in = PMI__Form(top, Title='iBEAt MTR analysis', [$
		  ptr_new({Type:'DROPLIST',Tag:'ser', Label:'MT series', Value:DynSeries, Select:in.ser}), $
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
    	Win = replicate({p:[0L,0L], n:d[0:1]}, d[2])
    	IF in.roi GT 0 THEN BEGIN
    	  Win.n *= 0
    	  Region = Stdy->Obj(1,in.roi-1)
    	  dim = Region->d()
    	  if dim[3] gt 1 then begin
    	    msg = 'Region of interest must be drawn on a static image'
    	    goto, jump
    	  endif
    	  for k=0L, d[2]-1 do begin
    	    Indices = Region->Where(Stdy->DataPath(), k, n=cnt)
			if cnt gt 0 then begin
    	    	Pos = ARRAY_INDICES(d[0:1], Indices, /DIMENSIONS)
    	    	Win[k].p = [$
    	      		min(Pos[0,*]),$
    	      		min(Pos[1,*])]
    	    	Win[k].n = [$
    	      		1+max(Pos[0,*])-min(Pos[0,*]),$
    	      		1+max(Pos[1,*])-min(Pos[1,*])]
    	    endif
    	  endfor
    	ENDIF
		return, 1
        JUMP: IF 'Cancel' EQ dialog_message(msg,/information,/cancel) THEN return, 0

  	ENDWHILE
END





pro PMI__Button__Event__iBEAt_MTR, ev

	PMI__Info, ev.top, Status=Status, Stdy=Stdy
    IF NOT PMI__Button__Input__iBEAt_MTR(ev.top,series,aif,in,moco,win) THEN RETURN

	PMI__Message, status, 'Calculating'

	d = Series -> d()

    independent = 0


	;Define new image series

	IF NOT in.no_moco THEN $
    Corr 		= Stdy -> New('SERIES', Default = Series, Name = Series->name()+'[MoCo]' )
    MTR   	= Stdy -> New('SERIES', Default = Series, Name = Series->name()+'[MT Map (%)]' )


	;Set time coordinates

    MTR   	-> t, Series->t(0)


	;Set default windowing

	MTR 	-> Trim, [0,80]

	start_time = systime(1)

	;Loop over all slices

    for k=0L,d[2]-1 do begin

		PMI__Message, status, 'Calculating', k/(d[2]-1E)

  		Par = FLTARR(d[0],d[1],1)

        MT_Map = FLTARR(d[0],d[1],1)

  		Source = Series->Read(Stdy->DataPath(), k, -1)

    	if product(win[k].n) gt 0 then begin

	        Source = TRANSPOSE(Source, [2,0,1])

            IF NOT in.no_moco THEN MOCOMO, source, 'MTR', Independent, GRID_SIZE=moco.res, TOLERANCE=moco.prec, WINDOW=win[k]
            Fit = MoCoModelFit(Source, 'MTR' , Independent, PARAMETERS=Par)

            MT_Map = 100*((Source[0,*,*]-Source[1,*,*])/Source[0,*,*]) ;100*((MT_OFF-MT_ON)/MT_OFF))


            Source = TRANSPOSE(Source, [1,2,0])
          ;  Par = TRANSPOSE(Par, [1,2,0])

    	endif

		;Write results to disk

		IF NOT in.no_moco THEN $
		Corr 		-> Write, Stdy->DataPath(), Source, k, -1
	;	MTR 	    -> Write, Stdy->DataPath(), Par[*,*,0], k
		MTR 	    -> Write, Stdy->DataPath(), MT_Map, k


	endfor

	print, 'calculation time (min): ', (systime(1)-start_time)/60.

    PMI__Control, ev.top, /refresh
end



pro PMI__Button__Control__iBEAt_MTR, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		sensitive = (ns gt 0)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end



function PMI__Button__iBEAt_MTR, parent,value=value,separator=separator

;	MoCoModel_MTR__DEFINE
    MoCoModel_Constant__DEFINE

    if n_elements(value) eq 0 then value = 'MT motion correction'

    id = widget_button(parent $
    ,   value = value  $
    ,  	event_pro = 'PMI__Button__Event__iBEAt_MTR' $
    ,	pro_set_value = 'PMI__Button__Control__iBEAt_MTR' $
    ,  	separator = separator )

    return, id
end

