;
;
;    Copyright (C) 2018 Steven Sourbron
;
;
;

FUNCTION PMI__Button__Input__MOCOMO_2D_DCE_1CMD, top, series, aif, in, Win

    PMI__Info, top, Stdy=Stdy
    DynSeries = Stdy->Names(0,DefDim=3,ind=ind,sel=sel)
	in = {ser:sel, aif:stdy->sel(1), roi:0L, res:16E, prec:1E, nb:1}

	WHILE 1 DO BEGIN

		in = PMI__Form(top, Title='Motion correction setup', [$
		ptr_new({Type:'DROPLIST',Tag:'ser', Label:'Dynamic series', Value:DynSeries, Select:in.ser}), $
		ptr_new({Type:'DROPLIST',Tag:'aif', Label:'Arterial Region', Value:Stdy->names(1), Select:in.aif}), $
		ptr_new({Type:'VALUE'	,Tag:'nb' , Label:'Length of baseline (# of dynamics)', Value:in.nb}), $
		ptr_new({Type:'DROPLIST',Tag:'roi', Label:'Region of Interest', Value:['<ENTIRE FOV>',Stdy->names(1)], Select:in.roi}), $
		ptr_new({Type:'VALUE'	,Tag:'res', Label:'Deformation Field Resolution (pixel sizes)', Value:in.res}), $
		ptr_new({Type:'VALUE'	,Tag:'prec', Label:'Deformation Field Precision (pixel sizes)', Value:in.prec}) $
		])
		IF in.cancel THEN return, 0

    	IF in.res LE 0 THEN BEGIN
    	  in.res = 16E
    	  msg = 'Deformation Field Resolution must be > 0'
    	  goto, jump
    	ENDIF
    	IF in.prec LE 0 THEN BEGIN
    	  in.prec = 1E
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
    	Aif = LMU__Enhancement(Aif,in.nb,relative=0)/(1-0.45)
		return, 1
        JUMP: IF 'Cancel' EQ dialog_message(msg,/information,/cancel) THEN return, 0

  	ENDWHILE
END

pro PMI__Button__Event__MOCOMO_2D_DCE_1CMD, ev

	PMI__Info, ev.top, Status=Status, Stdy=Stdy

    IF NOT PMI__Button__Input__MOCOMO_2D_DCE_1CMD(ev.top,series,aif,in,win) THEN RETURN

	PMI__Message, status, 'Calculating'

	time = Series->t() - Series->t(0)
	d = Series -> d()
    Corr = Stdy -> New('SERIES', Default = Series, Name = Series->name()+'[Motion-free]' )

tt = systime(1)

    for k=0L,d[2]-1 do begin

		PMI__Message, status, 'Calculating', k/(d[2]-1E)
  		Source = Series->Read(Stdy->DataPath(), k, -1)
    	if product(win[k].n) gt 0 then begin
	        Source = TRANSPOSE(Source, [2,0,1])
	        Source = MOCOMO(Source, 'QIM_DCE_1CMD', [Time, aif, in.nb], in.res, in.prec, Win=win[k])
            Source = TRANSPOSE(Source, [1,2,0])
		endif
		Corr -> Write, Stdy->DataPath(), Source, k, -1
	endfor

print,(systime(1)-tt)/60.

    PMI__Control, ev.top, /refresh
end


pro PMI__Button__Control__MOCOMO_2D_DCE_1CMD, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 0)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
    widget_control, id, sensitive=0 ;remove when debugged
end

function PMI__Button__MOCOMO_2D_DCE_1CMD, parent,value=value,separator=separator

	MOCOMO_3D__DEFINE
	MoCoModel__DEFINE
    MoCoModel_OneCompartment__DEFINE

    if n_elements(value) eq 0 then value = 'PK motion correction'

    id = widget_button(parent $
    ,   value = value  $
    ,  	event_pro = 'PMI__Button__Event__MOCOMO_2D_DCE_1CMD' $
    ,	pro_set_value = 'PMI__Button__Control__MOCOMO_2D_DCE_1CMD' $
    ,  	separator = separator )

    return, id
end

