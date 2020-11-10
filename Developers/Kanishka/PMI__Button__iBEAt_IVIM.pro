

FUNCTION PMI__Button__Input__iBEAt_IVIM, top, series, aif, in, moco, Win

    PMI__Info, top, Stdy=Stdy
    DynSeries = Stdy->Names(0,DefDim=3,ind=ind,sel=sel)
	in = {ser:sel, roi:0L, no_moco:1B}
	moco = {res:16E, prec:1E}

	WHILE 1 DO BEGIN

		in = PMI__Form(top, Title='iBEAt IVIM analysis', [$
		  ptr_new({Type:'DROPLIST',Tag:'ser', Label:'IVIM series', Value:DynSeries, Select:in.ser}), $
		  ptr_new({Type:'DROPLIST',Tag:'roi', Label:'Region of Interest', Value:['<ENTIRE FOV>',Stdy->names(1)], Select:in.roi}), $
		  ptr_new({Type:'DROPLIST',Tag:'no_moco', Label:'Perform motion correction?', Value:['Yes','No'], Select:in.no_moco}) $
		  ])
		IF in.cancel THEN return, 0

		IF NOT in.no_moco THEN BEGIN
		  moco = PMI__Form(top, Title='iBEAt IVIM MoCo settings', [$
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





pro PMI__Button__Event__iBEAt_IVIM, ev

	PMI__Info, ev.top, Status=Status, Stdy=Stdy
    IF NOT PMI__Button__Input__iBEAt_IVIM(ev.top,series,aif,in,moco,win) THEN RETURN

	PMI__Message, status, 'Calculating'

	;Get b-values and gradient vectors

	d = Series -> d()
    b = Series -> GETVALUE('0019'x,'100C'x)
    g = Series -> GETVALUE('0019'x,'100E'x)
    independent = {b:b, g:reform(g,[3,d[3]])}

	;Define new image series

	IF NOT in.no_moco THEN $
    Corr 		= Stdy -> New('SERIES', Default = Series, Name = Series->name()+'[MoCo]' )
    S0 			= Stdy -> New('SERIES', Default = Series, Name = Series->name()+'[S0]' )
    S0slow 		= Stdy -> New('SERIES', Default = Series, Name = Series->name()+'[S0 slow]' )
    S0fast 		= Stdy -> New('SERIES', Default = Series, Name = Series->name()+'[S0 fast]' )
    TensorSlow 	= Stdy -> New('SERIES', Default = Series, Name = Series->name()+'[Dslow_xx,yy,zz * 10-3 mm2/s]')
    TensorFast 	= Stdy -> New('SERIES', Default = Series, Name = Series->name()+'[Dfast_xx,yy,zz * 10-3 mm2/s]')
    ADCslow 	= Stdy -> New('SERIES', Default = Series, Name = Series->name()+'[ADCslow * 10-3 mm2/s]')
    ADCfast 	= Stdy -> New('SERIES', Default = Series, Name = Series->name()+'[ADCfast * 10-3 mm2/s]')
    FastFrac 	= Stdy -> New('SERIES', Default = Series, Name = Series->name()+'[Fast Fraction]')

	;Set time coordinates

    S0 			-> t, Series->t(0)
    S0slow 		-> t, Series->t(0)
    S0fast 		-> t, Series->t(0)
    TensorSlow 	-> t, Series->t(0) * (1+FINDGEN(3))
    TensorFast 	-> t, Series->t(0) * (1+FINDGEN(3))
    ADCslow		-> t, Series->t(0)
    ADCfast		-> t, Series->t(0)
    FastFrac	-> t, Series->t(0)

	;Set default windowing

	TensorSlow 	-> Trim, [0,3]
	TensorFast 	-> Trim, [0,10]
	ADCslow		-> Trim, [0,3]
	ADCfast 	-> Trim, [0,10]
	FastFrac	-> Trim, [0,1]

	start_time = systime(1)

	;Loop over all slices

    for k=0L,d[2]-1 do begin

		PMI__Message, status, 'Calculating', k/(d[2]-1E)

  		Par = FLTARR(d[0],d[1],8)
  		Map = FLTARR(d[0],d[1],4)

  		Source = Series->Read(Stdy->DataPath(), k, -1)

    	if product(win[k].n) gt 0 then begin

	        Source = TRANSPOSE(Source, [2,0,1])

            ;OLD
	       ; MOCOMO_2D, source, 'IVIM', Independent, $
	      ;    GRID_SIZE=moco.res, TOLERANCE=moco.prec, WINDOW=win[k], PARAMETERS=Par, NO_MOCO=in.no_moco

           IF NOT in.no_moco THEN MOCOMO, source, 'IVIM', Independent, GRID_SIZE=moco.res, TOLERANCE=moco.prec, WINDOW=win[k]
           Fit = MoCoModelFit(Source, 'IVIM' , Independent, PARAMETERS=Par)

            Source = TRANSPOSE(Source, [1,2,0])
            Par = TRANSPOSE(Par, [1,2,0])
            IVIM_Parameters, Par, Map

    	endif

		;Write results to disk

		IF NOT in.no_moco THEN $
		Corr 		-> Write, Stdy->DataPath(), Source, k, -1
		S0 			-> Write, Stdy->DataPath(), Map[*,*,0], k
		S0slow 		-> Write, Stdy->DataPath(), Par[*,*,0], k
		S0fast 		-> Write, Stdy->DataPath(), Par[*,*,4], k
		TensorSlow 	-> Write, Stdy->DataPath(), 1000*Par[*,*,1:3], k, -1
		TensorFast 	-> Write, Stdy->DataPath(), 1000*Par[*,*,5:7], k, -1 ; shouldn't this be 5:7?@ steven
		ADCslow 	-> Write, Stdy->DataPath(), 1000*Map[*,*,1], k
		ADCfast 	-> Write, Stdy->DataPath(), 1000*Map[*,*,2], k
		FastFrac 	-> Write, Stdy->DataPath(), Map[*,*,3], k

	endfor

	print, 'calculation time (min): ', (systime(1)-start_time)/60.

    PMI__Control, ev.top, /refresh
end





pro PMI__Button__Control__iBEAt_IVIM, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		sensitive = (ns gt 0)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end



function PMI__Button__iBEAt_IVIM, parent,value=value,separator=separator

	MoCoModel_IVIM__DEFINE

    if n_elements(value) eq 0 then value = 'IVIM motion correction'

    id = widget_button(parent $
    ,   value = value  $
    ,  	event_pro = 'PMI__Button__Event__iBEAt_IVIM' $
    ,	pro_set_value = 'PMI__Button__Control__iBEAt_IVIM' $
    ,  	separator = separator )

    return, id
end

