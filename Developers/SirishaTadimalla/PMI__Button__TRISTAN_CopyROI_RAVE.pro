
pro PMI__Button__Event__TRISTAN_CopyROI_RAVE, ev

	PMI__Info, ev.top, Stdy=Stdy, Region=region, Status=Status

	d = Region->d()

	v = PMI__Form(ev.top, Title='Shift Region..', [$
		ptr_new({Type:'VALUE', Tag:'s', Label:'Number of slices', Value:0L}) ])
	IF v.cancel THEN return

	targetSize = 36
	targetRows = 224
	targetCols = 224

	regionOutRAVE = bytarr(targetRows,targetCols,targetSize)
	im = Region->Read(Stdy->DataPath())
	print, d
	for n=0, targetSize-2-v.s do begin
		tmp = fltarr(d[0],d[0])
		tmp[*,(d[0]-d[1])/2:-1+(d[0]+d[1])/2] = im[*,*,n,0]
		regionOutRAVE[*,*,n+v.s] = CONGRID(tmp,targetCols, targetRows)
	endfor

	print, size(regionOutRAVE)


	RAVESeries = Stdy->Obj(0,where(Stdy->Names(0,DefDim=2,ind=ind,sel=sel) EQ 'RAVE3d1 _RAVE_VFA_T1 (ms)'))

	dom = {z:RAVESeries->z(), t:RAVESeries->t(0), m:RAVESeries->m()}
	Copy = Stdy->New('REGION' $
		,	Name 	= 'RAVE_'+Region->name() $
		,	Domain 	= dom $
		,	Color 	= Region->clr() $
		)

	Copy->Write, Stdy->DataPath(), regionOutRAVE

    exit: PMI__Control, ev.top, /refresh
end


pro PMI__Button__Control__TRISTAN_CopyROI_RAVE, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns)
		sensitive = ns gt 4
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__TRISTAN_CopyROI_RAVE, parent, value=value,separator=separator

    if n_elements(value) eq 0 then value = 'Copy ROIs to match FB dataset'

    id = widget_button(parent $
    ,   value = value  $
    ,  	event_pro = 'PMI__Button__Event__TRISTAN_CopyROI_RAVE' $
    ,	pro_set_value = 'PMI__Button__Control__TRISTAN_CopyROI_RAVE' $
    ,  	separator = separator )

    return, id
end

