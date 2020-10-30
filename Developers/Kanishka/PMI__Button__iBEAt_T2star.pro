

FUNCTION PMI__Button__Input__iBEAt_T2star, top, series, aif, in, moco, Win

    PMI__Info, top, Stdy=Stdy
    DynSeries = Stdy->Names(0,DefDim=3,ind=ind,sel=sel)
	in = {ser:sel, roi:0L, no_moco:1B}
	moco = {res:16E, prec:1E}

	WHILE 1 DO BEGIN

		in = PMI__Form(top, Title='iBEAt T2star analysis', [$
		  ptr_new({Type:'DROPLIST',Tag:'ser', Label:'T2star series', Value:DynSeries, Select:in.ser}), $
		  ptr_new({Type:'DROPLIST',Tag:'roi', Label:'Region of Interest', Value:['<ENTIRE FOV>',Stdy->names(1)], Select:in.roi}), $
		  ptr_new({Type:'DROPLIST',Tag:'no_moco', Label:'Perform motion correction?', Value:['Yes','No'], Select:in.no_moco}) $
		  ])
		IF in.cancel THEN return, 0

		IF NOT in.no_moco THEN BEGIN
		  moco = PMI__Form(top, Title='iBEAt T2star MoCo settings', [$
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





pro PMI__Button__Event__iBEAt_T2star, ev

	PMI__Info, ev.top, Status=Status, Stdy=Stdy
    IF NOT PMI__Button__Input__iBEAt_T2star(ev.top,series,aif,in,moco,win) THEN RETURN

	PMI__Message, status, 'Calculating'

	d = Series -> d()
    TE = Series -> GETVALUE('0018'x,'0081'x)
    independent = {TE:TE}

	;Define new image series

	IF NOT in.no_moco THEN $
    Corr 		= Stdy -> New('SERIES', Default = Series, Name = Series->name()+'[MoCo]' )
    S0 			= Stdy -> New('SERIES', Default = Series, Name = Series->name()+'[S0]' )
    T2star_fat 	= Stdy -> New('SERIES', Default = Series, Name = Series->name()+'[T2* fat (ms)]' )
    T2star_wat 	= Stdy -> New('SERIES', Default = Series, Name = Series->name()+'[T2* water (ms)]' )
    FatFrac 	= Stdy -> New('SERIES', Default = Series, Name = Series->name()+'[Fat fraction (%)]')

	;Set time coordinates

    S0 			-> t, Series->t(0)
    T2star_fat 	-> t, Series->t(0)
    T2star_wat 	-> t, Series->t(0)
    FatFrac 	-> t, Series->t(0)

	;Set default windowing

	T2star_fat 	-> Trim, [0,20]
	T2star_wat 	-> Trim, [0,50]
	FatFrac		-> Trim, [0,0.2]

	start_time = systime(1)

	;Loop over all slices

    for k=0L,d[2]-1 do begin

		PMI__Message, status, 'Calculating', k/(d[2]-1E)

  		Par = FLTARR(d[0],d[1],4)
  		Source = Series->Read(Stdy->DataPath(), k, -1)

    	if product(win[k].n) gt 0 then begin

	        Source = TRANSPOSE(Source, [2,0,1])

           IF NOT in.no_moco THEN MOCOMO, source, 'T2star', Independent, GRID_SIZE=moco.res, TOLERANCE=moco.prec, WINDOW=win[k]
           Fit = MoCoModelFit(Source, 'T2star' , Independent, PARAMETERS=Par)


            Source = TRANSPOSE(Source, [1,2,0])
            Par = TRANSPOSE(Par, [1,2,0])

    	endif

		;Write results to disk

		IF NOT in.no_moco THEN $
		Corr 		-> Write, Stdy->DataPath(), Source, k, -1
		S0 			-> Write, Stdy->DataPath(), Par[*,*,0]+Par[*,*,2], k
		T2star_fat 	-> Write, Stdy->DataPath(), Par[*,*,1], k
		T2star_wat 	-> Write, Stdy->DataPath(), Par[*,*,3], k
		FatFrac 	-> Write, Stdy->DataPath(), 100*Par[*,*,0]/(Par[*,*,0]+Par[*,*,2]), k

	endfor

	print, 'calculation time (min): ', (systime(1)-start_time)/60.

    PMI__Control, ev.top, /refresh
end





pro PMI__Button__Control__iBEAt_T2star, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		sensitive = (ns gt 0)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end



function PMI__Button__iBEAt_T2star, parent,value=value,separator=separator

	MoCoModel_T2star__DEFINE

    if n_elements(value) eq 0 then value = 'T2star motion correction'

    id = widget_button(parent $
    ,   value = value  $
    ,  	event_pro = 'PMI__Button__Event__iBEAt_T2star' $
    ,	pro_set_value = 'PMI__Button__Control__iBEAt_T2star' $
    ,  	separator = separator )

    return, id
end

