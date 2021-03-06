
pro PMI__Button__Event__HEPARIM_VFA, ev

	PMI__Info, ev.top, Stdy=Stdy, Status=Status
	PMI__Message, status, 'Getting user input..'

    Series = Stdy->Names(0,ns,DefDim=3,ind=ind,sel=sel)
	v = PMI__Form(ev.top, Title='VFA T1-mapping user input', [$
		ptr_new({Type:'DROPLIST',Tag:'series', Label:'Variable Flip Angle Series', Value:Series, Select:sel})$
		])
	IF v.cancel THEN goto, exit

	Series = Stdy->Obj(0,ind[v.series])

	Dom = {z:Series->z(), t:Series->t(0), m:Series->m()}
    S0_series = Stdy->New('SERIES', Domain= Dom,  Name= Series->name()+'_S0')
    R1_series = Stdy->New('SERIES', Domain= Dom,  Name= Series->name()+'_R1 (s-1)')
    T1_series = Stdy->New('SERIES', Domain= Dom,  Name= Series->name()+'_T1 (s)')
    FIT_series = Stdy->New('SERIES', Domain= Dom, Name= Series->name()+'_RMS (%)')

	TR = Series -> GETVALUE('0018'x,'0080'x) ;msec
	FA = Series -> t()

	d = Series->d()

	S0_slice = fltarr(d[0]*d[1])
	R1_slice = fltarr(d[0]*d[1])
	T1_slice = fltarr(d[0]*d[1])
	FIT_slice = fltarr(d[0]*d[1])

	for j=0L,d[2]-1 do begin ;loop over slices

		PMI__Message, status, 'Calculating ', j/(d[2]-1E)

		SVA_slice = Series->Read(Stdy->DataPath(),z=Series->z(j))
		SVA_slice = reform(SVA_slice,d[0]*d[1],d[3],/overwrite)

		for i=0L,d[0]*d[1]-1 do begin

			PAR = VFA_Linear_T1fit(TR, FA, reform(SVA_slice[i,*]), RMS = rms)

			S0_slice[i] = PAR[1]
			R1_slice[i] = Par[0]*1000
			T1_slice[i] = 1/R1_slice[i]
			Fit_slice[i] = rms

		endfor

		S0_series->Write, Stdy->DataPath(), S0_slice, j
		R1_series->Write, Stdy->DataPath(), R1_slice, j
		T1_series->Write, Stdy->DataPath(), T1_slice, j
		FIT_series->Write, Stdy->DataPath(), Fit_slice, j

	endfor

	S0_series->Trim, [0E, percentiles(S0_series->Read(Stdy->DataPath()),80)]
	R1_series->Trim, [0E, 2.0]
	T1_series->Trim, [0E, 2.0]
	FIT_series->Trim, [0E, 0.5]

    exit: PMI__Control, ev.top, /refresh
end


pro PMI__Button__Control__HEPARIM_VFA, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns)
		sensitive = ns gt 1
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__HEPARIM_VFA, parent, value=value,separator=separator

    if n_elements(value) eq 0 then value = 'VFA T1-mapping'

    id = widget_button(parent $
    ,   value = value  $
    ,  	event_pro = 'PMI__Button__Event__HEPARIM_VFA' $
    ,	pro_set_value = 'PMI__Button__Control__HEPARIM_VFA' $
    ,  	separator = separator )

    return, id
end

