

FUNCTION PMI__Button__Input__iBEAt_DCE, top, series, aif, in, moco, Win, const

	; T1 = ;default for kidney from de Bazelaire JMRI 2004
	; assuming 25% medulla and 75% cortex, so 1242 = 0.25*1142 + 0.75*1545
	relaxivity = 3.6 ;Hz/mM (DOTAREM)
	Hematocrit = 0.45
	T1_blood = 1.0 / (0.52 * Hematocrit + 0.38)  ; sec, Lu MRM 2004
	T1_Tissue = 1.242 ;sec
	const = {relaxivity:relaxivity, Hematocrit:Hematocrit, T1_blood:T1_blood, T1_tissue:T1_tissue}

    PMI__Info, top, Stdy=Stdy
    DynSeries = Stdy->Names(0,DefDim=3,ind=ind, sel=sel)
	in = {ser:sel, aif:stdy->sel(1), roi:0L, nb:1, no_moco:1B}
	moco = {res:16E, prec:1E}

	WHILE 1 DO BEGIN

		in = PMI__Form(top, Title='iBEAt DCE analysis', [$
		  ptr_new({Type:'DROPLIST',Tag:'ser', Label:'Dynamic series', Value:DynSeries, Select:in.ser}), $
		  ptr_new({Type:'DROPLIST',Tag:'aif', Label:'Arterial Region', Value:Stdy->names(1), Select:in.aif}), $
		  ptr_new({Type:'VALUE'	,Tag:'nb' , Label:'Length of baseline (# of dynamics)', Value:in.nb}), $
		  ptr_new({Type:'DROPLIST',Tag:'roi', Label:'Region of Interest', Value:['<ENTIRE FOV>',Stdy->names(1)], Select:in.roi}), $
		  ptr_new({Type:'DROPLIST',Tag:'no_moco', Label:'Perform motion correction?', Value:['Yes','No'], Select:in.no_moco}) $
		  ])
		IF in.cancel THEN return, 0

		IF NOT in.no_moco THEN BEGIN
		  moco = PMI__Form(top, Title='iBEAt DCE MoCo settings', [$
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
    	IF in.nb LT 1 THEN BEGIN
    	  in.nb = 10
    	  msg = ['Baseline length must be > 1',$
    		  'Please select another baseline length']
    	  goto, jump
    	ENDIF
    	IF in.nb GT d[3] THEN BEGIN
    	  in.nb = 10
    	  msg = ['Baseline length must be less than the total number of dynamics',$
    		  'Please select another baseline length']
    	  goto, jump
    	ENDIF
	   	Aif = PMI__RoiCurve(Stdy->DataPath(), Series, Stdy->Obj(1,in.aif), status, cnt=cnt)
    	IF cnt EQ 0 THEN BEGIN
    	  msg = ['Arterial region is empty on this series',$
    			'Please select another region and/or series']
    	  goto, jump
    	ENDIF
    	IF n_elements(Aif) NE d[3] THEN BEGIN
    	  msg = ['Arterial region is not defined on every dynamic',$
    			'Please select another region and/or series']
    	  goto, jump
    	ENDIF
    	Aif = LMU__Enhancement(Aif,in.nb,relative=1)
    	Aif /= const.relaxivity*const.T1_blood
    	Aif /= 1-const.Hematocrit

		return, 1
        JUMP: IF 'Cancel' EQ dialog_message(msg,/information,/cancel) THEN return, 0

  	ENDWHILE
END



pro PMI__Button__Event__iBEAt_DCE, ev

	PMI__Info, ev.top, Status=Status, Stdy=Stdy
    IF NOT PMI__Button__Input__iBEAt_DCE(ev.top,series,aif,in,moco,win,const) THEN RETURN

	PMI__Message, status, 'Calculating'

	;Get aif and time points

	d = Series -> d()
	time = Series->t() - Series->t(0)
	independent = {t:time, ca:aif, n0:in.nb}

    ;Define new image series

    IF NOT in.no_moco THEN $
    Corr = Stdy -> New('SERIES', Default = Series, Name = Series->name()+'[MoCo]' )
    S0 	 = Stdy -> New('SERIES', Default = Series, Name = Series->name()+'[Baseline]')
    VD   = Stdy -> New('SERIES', Default = Series, Name = Series->name()+'[Volume of Distribution (%)]')
    FB 	 = Stdy -> New('SERIES', Default = Series, Name = Series->name()+'[Blood Flow (mL/min/100mL)]')
    TB 	 = Stdy -> New('SERIES', Default = Series, Name = Series->name()+'[Blood MTT (sec)]')
    VB 	 = Stdy -> New('SERIES', Default = Series, Name = Series->name()+'[Blood Volume (%)]')
    FT 	 = Stdy -> New('SERIES', Default = Series, Name = Series->name()+'[Tubular Flow (mL/min/100mL)]')
    TT 	 = Stdy -> New('SERIES', Default = Series, Name = Series->name()+'[Tubular MTT (min)]')
    VT 	 = Stdy -> New('SERIES', Default = Series, Name = Series->name()+'[Apparent Tubular Volume (%)]')
    FF	 = Stdy -> New('SERIES', Default = Series, Name = Series->name()+'[Filtration Fraction (%)]')
    GF	 = Stdy -> New('SERIES', Default = Series, Name = Series->name()+'[GFR density (mL/min/100mL)]')

	;Set time coordinates

	S0 	-> t, Series->t(0)
	VD 	-> t, Series->t(0)
    FB 	-> t, Series->t(0)
    TB 	-> t, Series->t(0)
    VB 	-> t, Series->t(0)
    FT 	-> t, Series->t(0)
    TT 	-> t, Series->t(0)
    VT 	-> t, Series->t(0)
    FF 	-> t, Series->t(0)
    GF 	-> t, Series->t(0)

	;Set default windowing

	VD 	-> Trim, [0,100]
	FB 	-> Trim, [0,400]
	TB 	-> Trim, [0,30]
	VB 	-> Trim, [0,100]
	FT 	-> Trim, [0,20]
	TT 	-> Trim, [0,3]
	VT 	-> Trim, [0,50]
	FF 	-> Trim, [0,15]
	GF 	-> Trim, [0,20]

	start_time = systime(1)

	;Loop over all slices

    for k=0L,d[2]-1 do begin

		PMI__Message, status, 'Calculating', k/(d[2]-1E)

		Par = FLTARR(d[0],d[1],4)
  		Source = Series->Read(Stdy->DataPath(), k, -1)

    	if product(win[k].n) gt 0 then begin

	        Source = TRANSPOSE(Source, [2,0,1]) ;time to the front
	        MOCOMO_2D, Source, 'TwoCompartmentFiltration', independent, $
	          GRID_SIZE=moco.res, TOLERANCE=moco.prec, WINDOW=win[k], PARAMETERS=Par, NO_MOCO=in.no_moco
            Source = TRANSPOSE(Source, [1,2,0]) ;time to the back
            Par = PHYSICAL_2CFM_PARS(Par)
            Par = TRANSPOSE(Par, [1,2,0])

    	endif

		IF independent.n0 EQ 1 $
		THEN S0k = REFORM(Source[*,*,0]) $
		ELSE S0k = TOTAL(Source[*,*,0:independent.n0-1],3)/independent.n0

		scale = S0k * const.T1_tissue * const.relaxivity
		Par[*,*,0] /= scale
		Par[*,*,2] /= scale

		;Write results to disk

		IF NOT in.no_moco THEN $
		Corr 	-> Write, Stdy->DataPath(), Source, k, -1
		S0 		-> Write, Stdy->DataPath(), S0k, k
		VD 		-> Write, Stdy->DataPath(), 100*(Par[*,*,0]*Par[*,*,1]+Par[*,*,2]*Par[*,*,3]), k
		FB 		-> Write, Stdy->DataPath(), 6000*Par[*,*,0]/(1-const.Hematocrit), k
		TB 		-> Write, Stdy->DataPath(), Par[*,*,1], k
		VB		-> Write, Stdy->DataPath(), 100*Par[*,*,1]*Par[*,*,0]/(1-const.Hematocrit), k
		FT 		-> Write, Stdy->DataPath(), 6000*Par[*,*,2], k
		TT 		-> Write, Stdy->DataPath(), Par[*,*,3]/60, k
		VT 		-> Write, Stdy->DataPath(), 100*Par[*,*,3]*Par[*,*,2], k
		FF		-> Write, Stdy->DataPath(), 100*Par[*,*,2]/(Par[*,*,0]+Par[*,*,2]), k
		GF		-> Write, Stdy->DataPath(), 6000*Par[*,*,0]*Par[*,*,2]/(Par[*,*,0]+Par[*,*,2]), k

	endfor

	print, 'calculation time (min): ', (systime(1)-start_time)/60.

    PMI__Control, ev.top, /refresh
end


pro PMI__Button__Control__iBEAt_DCE, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 0)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end


function PMI__Button__iBEAt_DCE, parent,value=value,separator=separator

	MoCoModel_TwoCompartmentFiltration__DEFINE

    if n_elements(value) eq 0 then value = 'PK motion correction'

    id = widget_button(parent $
    ,   value = value  $
    ,  	event_pro = 'PMI__Button__Event__iBEAt_DCE' $
    ,	pro_set_value = 'PMI__Button__Control__iBEAt_DCE' $
    ,  	separator = separator )

    return, id
end

