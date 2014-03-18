function PMI__Button__MalihaSemiQuantitative__input $
   ,    ev $
   ,    Stdy = Stdy $
   ,    status = status $
   ,    time = time $
   ,    Series = series $
   ,  	nB = nB $
   ,	t1 = t1

    PMI__Info, ev.top, Status=Status, Stdy=Stdy

    Series = Stdy->Names(0,ns,DefDim=3,ind=ind,sel=sel)

    in = cw_InputForm(/pos, ev=ev               $
      ,	Title  = 'Calculate AUC' $
    ,  Labels  =    $
       [ 'Dynamic series' $
       , 'Length of baseline (# of dynamics)'    $
       , 'Last time point (sec)'$
       ]                               $
    ,  ListNames      = [ Series]$
    ,  ListNumbers  	= [ ns]$
    ,  ListDefaults     = [ sel]$
    ,  DataDefaults     = { nb:10L, t1:90E})
    if size(in,/type) eq 1 then return, 0

    Series = Stdy->Obj(0,ind[in.select[0]])
    nB = in.data.nb
    t1 = in.data.t1

    Time = Series->c(1)
    Time = Time-Time[0]

  	return, 1
end



pro PMI__Button__MalihaSemiQuantitative__event, ev

    if not PMI__Button__MalihaSemiQuantitative__input( $
       ev $
    ,  Stdy = Stdy $
    ,  status = status $
    ,  time=time $
    ,  Series = series $
    ,  nB = nB $
    ,  t1=t1 $
    )  then return

	t_ind = where(time lt t1, cnt)
    if cnt eq 0 then begin
    	ok = dialog_message(/information,'No data at these times')
    	return
    end

	Dom = {z:Series->z(), t:Series->t(0), m:Series->m()}
    Svd = Stdy->New('SERIES', Domain= Dom,  Name= 'Area under the curve [a.u.*min]')

	d = Series->d()
	P = fltarr(d[0]*d[1],cnt)

	for j=0L,d[2]-1 do begin

		PMI__Message, status, 'Calculating', j/(d[2]-1E)

		for k=0L,cnt-1 do P[*,k] = Series->Read(Stdy->DataPath(),j,k)

		if nB eq 1 then P0 = reform(P[*,0]) else P0 = total(P[*,0:nB-1],2)/nB
    	P = P - rebin(P0,d[0]*d[1],n_elements(P[0,*]))

		VD = float(time[1]*total(P,2)/60)
		Svd->Write, Stdy->DataPath(), VD, j
	endfor

	Svd -> Trim, [0,0.8*max(VD)]

    PMI__Control, ev.top, /refresh
end


pro PMI__Button__Control__MalihaSemiQuantitative, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		sensitive = ns gt 0
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__MalihaSemiQuantitative, parent,value=value,separator=separator

    if n_elements(value) eq 0 then value = 'Area under the curve (Pixel)'

    id = widget_button(parent                    $
    ,   value      = value   $
    ,  event_pro   = 'PMI__Button__MalihaSemiQuantitative__event'       $
    ,	pro_set_value 	= 'PMI__Button__Control__MalihaSemiQuantitative' $
    ,  separator   = separator                    )

    return, id
end

