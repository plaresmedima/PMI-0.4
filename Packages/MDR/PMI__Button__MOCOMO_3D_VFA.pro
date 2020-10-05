FUNCTION PMI__Button__Input__MOCOMO_3D_VFA, top, series, aif, in, Win

    PMI__Info, top, Stdy=Stdy
    SeriesNames = Stdy->Names(0)
    in = {vfa:0L, res:16E, prec:1E}

	WHILE 1 DO BEGIN

		in = PMI__Form(top, Title='VFA Motion correction setup', [$
		ptr_new({Type:'LIST'	,Tag:'vfa', Label:'Select VFA series', Value:SeriesNames, Select:in.vfa}), $
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

		return, 1
        JUMP: IF 'Cancel' EQ dialog_message(msg,/information,/cancel) THEN return, 0

  	ENDWHILE
END

pro PMI__Button__Event__MOCOMO_3D_VFA, ev

	PMI__Info, ev.top, Status=Status, Stdy=Stdy
	PMI__Message, status, 'Preparing calculation..'

    IF NOT PMI__Button__Input__MOCOMO_3D_VFA(ev.top,series,aif,in,win) THEN RETURN

	PMI__Message, status, 'Preparing calculation..'

	time = Series->t() - Series->t(0)
	Source = Series->Read(Stdy->DataPath())

    PMI__Message, status, 'Calculating..'
tt=systime(1)
	Source = TRANSPOSE(Source, [3,0,1,2])
	Source = MOCOMO_3D(Source, 'QIM_VFA', [Time, aif, in.nb], in.res, in.prec, Win=win)
    Source = TRANSPOSE(Source, [1,2,3,0])
    Dom = {z:Series->z(), t:Series->t(), m:Series->m()}
    Corr = Stdy->New('SERIES', Domain=Dom,  Name=Series->name() + '[Motion-free]' )
	Corr -> Write, Stdy->DataPath(), Source
	Corr -> Trim, Series->Trim()
print,(systime(1)-tt)/60.

    PMI__Control, ev.top, /refresh
end


pro PMI__Button__Control__MOCOMO_3D_VFA, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 0)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__MOCOMO_3D_VFA, parent,value=value,separator=separator

	QIM_VFA ;Model to fit

    if n_elements(value) eq 0 then value = 'PK motion correction'

    id = widget_button(parent $
    ,   value = value  $
    ,  	event_pro = 'PMI__Button__Event__MOCOMO_3D_VFA' $
    ,	pro_set_value = 'PMI__Button__Control__MOCOMO_3D_VFA' $
    ,  	separator = separator )

    return, id
end

