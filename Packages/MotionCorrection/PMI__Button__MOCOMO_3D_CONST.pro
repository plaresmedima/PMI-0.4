;
;
;    Copyright (C) 2018 Steven Sourbron
;


FUNCTION PMI__Button__Input__MOCOMO_3D_CONST, top, series, in, Win

    PMI__Info, top, Stdy=Stdy
    DynSeries = Stdy->Names(0,DefDim=3,ind=ind,sel=sel)
	in = {ser:sel, roi:0L, res:16E, prec:1E}

	WHILE 1 DO BEGIN

		in = PMI__Form(top, Title='Motion correction setup', [$
		ptr_new({Type:'DROPLIST',Tag:'ser', Label:'Dynamic series', Value:DynSeries, Select:in.ser}), $
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



pro PMI__Button__Event__MOCOMO_3D_CONST, ev

	PMI__Info, ev.top, Status=Status, Stdy=Stdy
	PMI__Message, status, 'Preparing calculation..'

    IF NOT PMI__Button__Input__MOCOMO_3D_CONST(ev.top,series,in,win) THEN RETURN

	PMI__Message, status, 'Preparing calculation..'

	time = Series->t() - Series->t(0)
	Source = Series->Read(Stdy->DataPath())

    PMI__Message, status, 'Calculating..'
tt=systime(1)
	Source = TRANSPOSE(Source, [3,0,1,2])
    MOCOMO = OBJ_NEW('MOCOMO_3D_CONST', ptr_new(Source), in.res, in.prec, 0B, Win=win)
    Source = TRANSPOSE(MOCOMO->deformed(), [1,2,3,0])
    OBJ_DESTROY, MOCOMO
    Dom = {z:Series->z(), t:Series->t(), m:Series->m()}
    Corr = Stdy->New('SERIES', Domain=Dom,  Name=Series->name() + '[Motion-free]' )
	Corr -> Write, Stdy->DataPath(), Source
	Corr -> Trim, Series->Trim()
print,(systime(1)-tt)/60.

    PMI__Control, ev.top, /refresh
end


pro PMI__Button__Control__MOCOMO_3D_CONST, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		sensitive = ns gt 0
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__MOCOMO_3D_CONST, parent,value=value,separator=separator

	MOCOMO_3D_CONST__DEFINE

    if n_elements(value) eq 0 then value = 'Dynamic motion correction'

    id = widget_button(parent $
    ,   value = value  $
    ,  	event_pro = 'PMI__Button__Event__MOCOMO_3D_CONST' $
    ,	pro_set_value = 'PMI__Button__Control__MOCOMO_3D_CONST' $
    ,  	separator = separator )

    return, id
end

