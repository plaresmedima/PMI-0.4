function PMI__Button__MalihaDeconvolution__input $
   ,    ev $
   ,    Stdy = Stdy $
   ,    status = status $
   ,    time = time $
   ,    aif = aif $
   ,    Series = series $
   ,  	nB = nB $
   ,	t1 = t1

    PMI__Info, ev.top, Status=Status, Stdy=Stdy

    Series = Stdy->Names(0,ns,DefDim=3,ind=ind,sel=sel)
    Regions = Stdy->names(1,nr)

    in = cw_InputForm(/pos, ev=ev               $
      ,	Title  = 'Deconvolution analysis setup' $
    ,  Labels  =    $
       [ 'Dynamic series' $
       , 'Arterial region'   $
       , 'Length of baseline (# of dynamics)'    $
       , 'Last time point (sec)'$
       ]                               $
    ,  ListNames      = [ Series,Regions]$
    ,  ListNumbers  	= [ ns,nr]$
    ,  ListDefaults     = [ sel,Stdy->sel(1)]$
    ,  DataDefaults     = { nb:10L, t1:45E})
    if size(in,/type) eq 1 then return, 0

    Series = Stdy->Obj(0,ind[in.select[0]])
    Art = Stdy->Obj(1,in.select[1])
    nB = in.data.nb
    t1 = in.data.t1

    Time = Series->c(1)
    Time = Time-Time[0]

    Aif = PMI__RoiCurve(Stdy->DataPath(), Series, Art, status, cnt=cnt)
    if cnt eq 0 then begin
    	ok = dialog_message(/information,'Arterial region is empty')
    	return, 0
    end
    if n_elements(Aif) ne n_elements(Time) then begin
    	ok = dialog_message(/information,'Arterial region is not defined on every dynamic')
    	return, 0
    end
    Aif = LMU__Enhancement(Aif,nB,relative = 0)/(1-0.45)

  	return, 1
end



pro PMI__Button__MalihaDeconvolution__event, ev

    if not PMI__Button__MalihaDeconvolution__input( $
       ev $
    ,  Stdy = Stdy $
    ,  status = status $
    ,  time = time $
    ,  aif = aif $
    ,  Series = series $
    ,  nB = nB $
    ,  t1=t1 $
    )  then return

	t_ind = where(time lt t1, cnt)
    if cnt eq 0 then begin
    	ok = dialog_message(/information,'No data at these times')
    	return
    end
    time = time[t_ind]
    aif = aif[t_ind]

	Dom = {z:Series->z(), t:Series->t(0), m:Series->m()}

    Svd = Stdy->New('SERIES', Domain= Dom,  Name= 'Volume of Distribution (ml/100ml)')
    Stt = Stdy->New('SERIES', Domain= Dom,  Name= 'Mean Transit Time (sec)' )
    Spf = Stdy->New('SERIES', Domain= Dom,  Name= 'Plasma Flow (ml/100ml/min)' )

	d = Series->d()
	P = fltarr(d[0]*d[1],cnt)

	for j=0L,d[2]-1 do begin

		PMI__Message, status, 'Calculating', j/(d[2]-1E)

		for k=0L,cnt-1 do P[*,k] = Series->Read(Stdy->DataPath(),j,k)

		if nB eq 1 then P0 = reform(P[*,0]) else P0 = total(P[*,0:nB-1],2)/nB
    	P = P - rebin(P0,d[0]*d[1],n_elements(P[0,*]))

		InvertIllPosedFast, P, time[1]*convolution_matrix(aif), REGPAR=0.15

		PF = 6000*float(max(P,dimension=2))
		VD = 100*float(time[1]*total(P,2))
		TT = float(remove_inf(VD/PF))

		Spf->Write, Stdy->DataPath(), PF, j
		Svd->Write, Stdy->DataPath(), VD, j
		Stt->Write, Stdy->DataPath(), TT, j

	endfor

	Spf -> Trim, [0,0.8*max(PF)]
	Svd -> Trim, [0,0.8*max(VD)]
	Stt -> Trim, [0,0.8*max(TT)]

    PMI__Control, ev.top, /refresh
end


pro PMI__Button__Control__MalihaDeconvolution, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 0)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__MalihaDeconvolution, parent,value=value,separator=separator

    if n_elements(value) eq 0 then value = 'Deconvolution analysis (Pixel)'

    id = widget_button(parent                    $
    ,   value      = value   $
    ,  event_pro   = 'PMI__Button__MalihaDeconvolution__event'       $
    ,	pro_set_value 	= 'PMI__Button__Control__MalihaDeconvolution' $
    ,  separator   = separator                    )

    return, id
end

