
pro PMI__Button__Event__TRISTAN_CopyCoronalROIs, ev

	PMI__Info, ev.top, Stdy=Stdy, Region=region, Status=Status

	d = Region->d()
	im = Region->Read(Stdy->DataPath())

	; Get user input on shift in slice direction needed for
	; each dataset to match the given region
	v = PMI__Form(ev.top, Title='Slice offset from given region', [$
		ptr_new({Type:'VALUE', Tag:'sBH', Label:'BH', Value:0L}),$
		ptr_new({Type:'VALUE', Tag:'sFB' , Label:'FB', Value:0L})])
	IF v.cancel THEN return

	ser = ['BH','FB']
	for s=0, n_elements(ser)-1 do begin
		case ser[s] of
			'BH': begin
					targetSize = 72
					targetRows = d[0]
					targetCols = d[1]
					SeriesType = '*fl3d1_BH_VFA_T1 (ms)'
					Name = Region->name()+'_BH'
					regionOut = bytarr(targetRows,targetCols,targetSize,d[3])
					for n=1, targetSize/2-1 do begin
						k = 2*n-1+v.sBH
						regionOut[*,*,k,0] = im[*,*,n,0]
						regionOut[*,*,k+1,0] = im[*,*,n,0]
					endfor
				  end
			'FB': begin
					targetSize = 36
					targetRows = 96
					targetCols = 78
					SeriesType = '*fl3d1_FB_VFA_T1 (ms)'
					Name = Region->name()+'_FB'
					regionOut = bytarr(targetRows,targetCols,targetSize)
					for n=0, targetSize-1-v.sFB do begin
						regionOut[*,*,n] = CONGRID(im[*,*,n+v.sFB,0],targetCols, targetRows)
					endfor
				  end
		endcase
		Series = Stdy->Obj(0,where(Stdy->Names(0,DefDim=2,ind=ind,sel=sel) EQ SeriesType))
		dom = {z:Series->z(), t:Series->t(0), m:Series->m()}
		Copy = Stdy->New('REGION' $
			,	Name 	= Name $
			,	Domain 	= dom $
			,	Color 	= Region->clr() $
			)

		Copy->Write, Stdy->DataPath(), regionOut
	endfor

    exit: PMI__Control, ev.top, /refresh
end


pro PMI__Button__Control__TRISTAN_CopyCoronalROIs, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns)
		sensitive = ns gt 4
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__TRISTAN_CopyCoronalROIs, parent, value=value,separator=separator

    if n_elements(value) eq 0 then value = 'Copy ROIs to match BH, FB datasets'

    id = widget_button(parent $
    ,   value = value  $
    ,  	event_pro = 'PMI__Button__Event__TRISTAN_CopyCoronalROIs' $
    ,	pro_set_value = 'PMI__Button__Control__TRISTAN_CopyCoronalROIs' $
    ,  	separator = separator )

    return, id
end

