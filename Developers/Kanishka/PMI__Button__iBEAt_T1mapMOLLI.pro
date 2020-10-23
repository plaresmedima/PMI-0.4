

FUNCTION PMI__Button__Input__iBEAt_T1mapMOLLI, top, series, aif, in, moco, Win

    PMI__Info, top, Stdy=Stdy
    DynSeries = Stdy->Names(0,DefDim=3,ind=ind,sel=sel)
	in = {ser:sel, roi:0L, no_moco:1B}
	moco = {res:16E, prec:1E}

	WHILE 1 DO BEGIN

		in = PMI__Form(top, Title='iBEAt T1mapMOLLI analysis', [$
		  ptr_new({Type:'DROPLIST',Tag:'ser', Label:'T1map series', Value:DynSeries, Select:in.ser}), $
		  ptr_new({Type:'DROPLIST',Tag:'roi', Label:'Region of Interest', Value:['<ENTIRE FOV>',Stdy->names(1)], Select:in.roi}), $
		  ptr_new({Type:'DROPLIST',Tag:'no_moco', Label:'Perform motion correction?', Value:['Yes','No'], Select:in.no_moco}) $
		  ])
		IF in.cancel THEN return, 0

		IF NOT in.no_moco THEN BEGIN
		  moco = PMI__Form(top, Title='iBEAt T1mapMOLLI MoCo settings', [$
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





pro PMI__Button__Event__iBEAt_T1mapMOLLI, ev

	PMI__Info, ev.top, Status=Status, Stdy=Stdy
    IF NOT PMI__Button__Input__iBEAt_T1mapMOLLI(ev.top,series,aif,in,moco,win) THEN RETURN

	PMI__Message, status, 'Calculating'

	d = Series -> d()
    TI = Series -> GETVALUE('0018'x,'0082'x)
    independent = {TI:TI}


	;Define new image series: Motion corrected series; T1 map MOLLI after fitting

	IF NOT in.no_moco THEN $
    Corr 		= Stdy -> New('SERIES', Default = Series, Name = Series->name()+'[MoCo]' )
    T1final 	= Stdy -> New('SERIES', Default = Series, Name = Series->name()+'[Estimated T1 Map (ms)]')
    A           = Stdy -> New('SERIES', Default = Series, Name = Series->name()+'[A]') ; A from the fitting
	B           = Stdy -> New('SERIES', Default = Series, Name = Series->name()+'[B]') ; B from the fitting
	T1mapMOLLIApp  = Stdy -> New('SERIES', Default = Series, Name = Series->name()+'[T1 Apparent (ms)]') ; T1 map apparent  from the fitting


	;Set time coordinates

    T1final 	    -> t, Series->t(0) ; 5 slices generated after fitting
    A	    	    -> t, Series->t(0)
    B	        	-> t, Series->t(0)
    T1mapMOLLIApp	-> t, Series->t(0)



	;Set default windowing

	T1final		    -> Trim, [0E, 2000] ; T1 map range: 1200-1800ms for cortex and medulla in HV
	A               -> Trim, [0E, 2000]
	B               -> Trim, [0E, 2000]
	T1mapMOLLIApp   -> Trim, [0E, 2000]



	start_time = systime(1)

	;Loop over all slices

    for k=0L,d[2]-1 do begin

		PMI__Message, status, 'Calculating', k/(d[2]-1E) ; loopin over slices; %age progress
		independent = {TI:TI[k,*]}

  		Par = FLTARR(d[0],d[1],3)

  		Source = Series->Read(Stdy->DataPath(), k, -1)

    	if product(win[k].n) gt 0 then begin

	        Source = TRANSPOSE(Source, [2,0,1])

            ; negative values from null point

	         for l=0L,d[0]-1 do begin ; dim x 384 dim[2,0,1]

	              for m = 0L,d[1]-1 do begin  ; dim y 384

	                  TI_Source = min(Source[*,l,m], indxx) ; find min index
	                  Source[0:indxx,l,m] = -  Source[0:indxx,l,m]

                   endfor

               endfor
;

           IF NOT KEYWORD_SET(no_moco) THEN MOCOMO, source, 'T1mapMOLLI', Independent, GRID_SIZE=moco.res, TOLERANCE=moco.prec, WINDOW=win[k]
           Fit = MoCoModelFit(Source, 'T1mapMOLLI' , Independent, PARAMETERS=Par)


            Source = TRANSPOSE(Source, [1,2,0])
            Par = TRANSPOSE(Par, [1,2,0])

    	endif

		;Write results to disk

		IF NOT in.no_moco THEN $
		Corr 		-> Write, Stdy->DataPath(), Source, k, -1 ; motion corrected series; if no motion correction then it is the same no motion corrected series
		A           -> Write, Stdy->DataPath(), Par[*,*,0], k ; Sinf_slice; A from the fitting for each slice k
		B           -> Write, Stdy->DataPath(), Par[*,*,1], k ; Sratio_slice; B from the fitting for each slice k
	;	T1mapMOLLIApp  -> Write, Stdy->DataPath(), 1/Par[*,*,2], k
		T1mapMOLLIApp  -> Write, Stdy->DataPath(), Par[*,*,2], k   ; T1_slice; T1 map apparent  from the fitting
	;	T1final 	-> Write, Stdy->DataPath(),(Par[*,*,1]-1)/Par[*,*,2],k;
		T1final 	-> Write, Stdy->DataPath(),((Par[*,*,1]/Par[*,*,0])-1)*Par[*,*,2], k ; T1 estimated: final T1 map for each slice

	endfor




	print, 'calculation time (min): ', (systime(1)-start_time)/60.

    PMI__Control, ev.top, /refresh
end





pro PMI__Button__Control__iBEAt_T1mapMOLLI, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		sensitive = (ns gt 0)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end



function PMI__Button__iBEAt_T1mapMOLLI, parent,value=value,separator=separator

	MoCoModel_T1mapMOLLI__DEFINE

    if n_elements(value) eq 0 then value = 'T1mapMOLLI motion correction'

    id = widget_button(parent $
    ,   value = value  $
    ,  	event_pro = 'PMI__Button__Event__iBEAt_T1mapMOLLI' $
    ,	pro_set_value = 'PMI__Button__Control__iBEAt_T1mapMOLLI' $
    ,  	separator = separator )

    return, id
end

