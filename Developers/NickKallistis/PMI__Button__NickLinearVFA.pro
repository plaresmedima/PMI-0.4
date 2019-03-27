
pro PMI__Button__Event__NickLinearVfa, ev

	PMI__Info, ev.top, Stdy=Stdy, Status=Status
	PMI__Message, status, 'Getting user input..'

    SeriesNames = Stdy->Names(0)

	in = PMI__Form(ev.top, Title='VFA T1-mapping input', [$
		ptr_new({Type:'LIST',Tag:'VFA', Label:'Select 5 VFA series', Value:SeriesNames, Select:0})])
	IF in.cancel THEN goto, exit

	sel = in.VFA
	nFA = n_elements(sel)

    SeriesFA0 = Stdy->Obj(0,sel[0])
    SeriesFA1 = Stdy->Obj(0,sel[1])
    SeriesFA2 = Stdy->Obj(0,sel[2])
    SeriesFA3 = Stdy->Obj(0,sel[3])
    SeriesFA4 = Stdy->Obj(0,sel[4])

	Dom = {z:SeriesFA0->z(), t:SeriesFA0->t(0), m:SeriesFA0->m()}
    S0_series = Stdy->New('SERIES', Domain= Dom,  Name= 'RAVE_VFA_S0')
    R1_series = Stdy->New('SERIES', Domain= Dom,  Name= 'RAVE_VFA_R1 (s-1)')
    T1_series = Stdy->New('SERIES', Domain= Dom,  Name= 'RAVE_VFA_T1 (s)')

	TR = seriesFA0->GETVALUE('0018'x,'0080'x)/1000.0 ;sec

	FA = [seriesFA0->GETVALUE('0018'x,'1314'x), $
		seriesFA1->GETVALUE('0018'x,'1314'x), $
		seriesFA2->GETVALUE('0018'x,'1314'x), $
		seriesFA3->GETVALUE('0018'x,'1314'x), $
		seriesFA4->GETVALUE('0018'x,'1314'x)]

	d = SeriesFA0->d()

	for j=0L,d[2]-1 do begin ;loop over slices

		PMI__Message, status, 'Calculating ', j/(d[2]-1E)

		S0 = SeriesFA0->Read(Stdy->DataPath(),z=SeriesFA0->z(j))
		S1 = SeriesFA1->Read(Stdy->DataPath(),z=SeriesFA1->z(j))
		S2 = SeriesFA2->Read(Stdy->DataPath(),z=SeriesFA2->z(j))
		S3 = SeriesFA3->Read(Stdy->DataPath(),z=SeriesFA3->z(j))
		S4 = SeriesFA4->Read(Stdy->DataPath(),z=SeriesFA4->z(j))

		S0_slice = fltarr(d[0]*d[1])
		R1_slice = fltarr(d[0]*d[1])
		T1_slice = fltarr(d[0]*d[1])

		for i=0L,d[0]*d[1]-1 do begin

			S_VFA = [S0[i],S1[i],S2[i],S3[i],S4[i]]

			PAR = VFA_Linear_T1fit(TR, FA, S_VFA)

			S0_slice[i] = PAR[1]
			R1_slice[i] = Par[0]
			T1_slice[i] = 1/Par[0]
		endfor

		S0_series->Write, Stdy->DataPath(), S0_slice, j
		R1_series->Write, Stdy->DataPath(), R1_slice, j
		T1_series->Write, Stdy->DataPath(), T1_slice, j

	endfor

	S0_series->Trim, [0E, max(S0_slice,/NAN)]
	R1_series->Trim, [0E, 2.0]
	T1_series->Trim, [0E, 2.0]

    exit: PMI__Control, ev.top, /refresh
end


pro PMI__Button__Control__NickLinearVfa, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns)
		sensitive = ns gt 4
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__NickLinearVfa, parent, value=value,separator=separator

	PMI__FitT1SaturationRecovery

    if n_elements(value) eq 0 then value = 'VFA T1-mapping'

    id = widget_button(parent $
    ,   value = value  $
    ,  	event_pro = 'PMI__Button__Event__NickLinearVfa' $
    ,	pro_set_value = 'PMI__Button__Control__NickLinearVfa' $
    ,  	separator = separator )

    return, id
end

