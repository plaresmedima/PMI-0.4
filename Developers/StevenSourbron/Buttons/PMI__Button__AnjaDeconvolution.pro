pro PMI__Button__Event__AnjaDeconvolution, ev

	PMI__Info, ev.top, Status=Status, Stdy=Stdy
	Series = Stdy->names(0,ns,DefDim=3,ind=ind,sel=sel)

	v = PMI__Form(ev.top, Title='Pixel analysis setup', [$
		ptr_new({Type:'DROPLIST',Tag:'ser', Label:'Dynamic series', Value:Series, Select:sel}), $
		ptr_new({Type:'DROPLIST',Tag:'aif', Label:'Arterial region', Value:Stdy->names(1), Select:Stdy->sel(1)}), $
		ptr_new({Type:'VALUE'	,Tag:'nb' , Label:'Length of baseline (# of dynamics)', Value:8L}) ])
		IF v.cancel THEN return

    if v.nb lt 2 then begin
    	ok = dialog_message(/information,'Baseline length must be 2 or more')
    	return
    end

	Series = Stdy->Obj(0,ind[v.ser])
	Time = Series->c(1)
	Time = Time[1:*]-Time[1]

  	Aif = PMI__RoiCurve(Stdy->DataPath(), Series, Stdy->Obj(1,v.aif), status, cnt=cnt)
    if cnt eq 0 then begin
    	ok = dialog_message(/information,'Arterial region is empty')
    	return
    end
	Aif = LMU__Enhancement(Aif[1:*],v.nb-1,/relative)/0.55

  	cw_create_plot, Aif $
  	,	xaxis 	= Time $
	,	xtitle 	= 'Time (/sec)' $
	,	ytitle 	= 'Relative Signal Enhancement' $
	,	title 	= 'Arterial Input Function'

	Dom = {z:Series->z(), t:Series->t(1), m:Series->m()}

	PF = Stdy->New('SERIES',Domain=Dom, ClrWin=[0E,40E]	, Name= 'Plasma Flow (ml/100ml/min)')
	VD = Stdy->New('SERIES',Domain=Dom, ClrWin=[0E,30E] , Name= 'Volume of Distribution (ml/100ml)')
	TT = Stdy->New('SERIES',Domain=Dom, ClrWin=[0E,200E] , Name= 'Mean Transit Time (sec)')

	d = Series->d()
	dt = float(time[1])
	Mat = dt*convolution_matrix(Aif)

	for i=0L,d[2]-1 do begin

		if d[2] gt 1 then PMI__Message, status, 'Calculating', i/(d[2]-1E)

		S = Series -> Read(Stdy->DataPath(),i,-1)
		S = reform(S,d[0]*d[1],d[3],/overwrite)
		S = S[*,1:*]

		if v.nB eq 2 then S0 = reform(S[*,0]) else S0 = total(S[*,0:v.nB-2],2)/(v.nb-1E)
		for j=0L,d[3]-2 do S[*,j]=S[*,j]/S0-1

		INVERTILLPOSEDFAST, S, Mat

		S = float(S)

		PFi = 6000*Max(S,Dimension=2)
		VDi = 100.0*dt*Total(S,2)
		TTi = 60*remove_inf(VDi/PFi)

		PF -> Write, Stdy->DataPath(), PFi, i
		VD -> Write, Stdy->DataPath(), VDi, i
		TT -> Write, Stdy->DataPath(), TTi, i
	endfor

	PMI__Control, ev.top, /refresh
end


pro PMI__Button__Control__AnjaDeconvolution, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 0)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end


function PMI__Button__AnjaDeconvolution, parent,value=value,separator=separator

	if n_elements(value) eq 0 then value = 'Pixel analysis'

    id = widget_button(parent $
    , 	value 		= value	$
    ,  	event_pro   = 'PMI__Button__Event__AnjaDeconvolution'       $
    ,	pro_set_value 	= 'PMI__Button__Control__AnjaDeconvolution' $
	, 	separator 	= separator	)

	return, id
end

