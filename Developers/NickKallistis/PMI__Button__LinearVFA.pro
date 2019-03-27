
pro PMI__Button__Event__LinearVfa, ev

	PMI__Info, ev.top, Stdy=Stdy, Status=Status
	PMI__Message, status, 'Getting user input..'

    SeriesNames = Stdy->Names(0)

	in = PMI__Form(ev.top, Title='VFA T1-mapping input', [$
		ptr_new({Type:'DROPLIST',Tag:'FA02', Label:'RAVE FA 2', Value:SeriesNames, Select:0}), $
		ptr_new({Type:'DROPLIST',Tag:'FA05', Label:'RAVE FA 5', Value:SeriesNames, Select:0}), $
		ptr_new({Type:'DROPLIST',Tag:'FA10', Label:'RAVE FA 10', Value:SeriesNames, Select:0}), $
		ptr_new({Type:'DROPLIST',Tag:'FA12', Label:'RAVE FA 12', Value:SeriesNames, Select:0}), $
		ptr_new({Type:'DROPLIST',Tag:'FA15', Label:'RAVE FA 15', Value:SeriesNames, Select:0})])
	IF in.cancel THEN goto, exit

    SeriesFA02 = Stdy->Obj(0,in.FA02)
    SeriesFA05 = Stdy->Obj(0,in.FA05)
    SeriesFA10 = Stdy->Obj(0,in.FA10)
    SeriesFA12 = Stdy->Obj(0,in.FA12)
    SeriesFA15 = Stdy->Obj(0,in.FA15)

	Dom = {z:SeriesFA02->z(), t:SeriesFA02->t(0), m:SeriesFA02->m()}
    S0_series = Stdy->New('SERIES', Domain= Dom,  Name= Series->name() + '_S0')
    R1_series = Stdy->New('SERIES', Domain= Dom,  Name= Series->name() + '_R1 (s-1)')

	TR = seriesFA02->GETVALUE('0018'x,'0080'x)/1000.0 ;sec

	FA = [seriesFA02->GETVALUE('0018'x,'1314'x), $
		seriesFA05->GETVALUE('0018'x,'1314'x), $
		seriesFA10->GETVALUE('0018'x,'1314'x), $
		seriesFA12->GETVALUE('0018'x,'1314'x), $
		seriesFA15->GETVALUE('0018'x,'1314'x)]

	d = SeriesFA02->d()

	for j=0L,d[2]-1 do begin ;loop over slices

		PMI__Message, status, 'Calculating ', j/(d[2]-1E)

		S02 = SeriesFA02->Read(Stdy->DataPath(),z=SeriesFA02->z(j))
		S05 = SeriesFA05->Read(Stdy->DataPath(),z=SeriesFA05->z(j))
		S10 = SeriesFA10->Read(Stdy->DataPath(),z=SeriesFA10->z(j))
		S12 = SeriesFA12->Read(Stdy->DataPath(),z=SeriesFA12->z(j))
		S15 = SeriesFA15->Read(Stdy->DataPath(),z=SeriesFA15->z(j))

		S0_slice = fltarr(d[0]*d[1])
		R1_slice = fltarr(d[0]*d[1])

		for i=0L,d[0]*d[1]-1 do begin

			S_VFA = [S02[i],S05[i],S10[i],S12[i],S15[i]]

			PAR = VFA_Linear_T1fit(TR, FA, S_VFA)

			S0_slice[i] = PAR[1]
			R1_slice[i] = Par[0]
		endfor

		S0_series->Write, Stdy->DataPath(), S0_slice, j
		R1_series->Write, Stdy->DataPath(), R1_slice, j

	endfor

	S0_series->Trim, [0E, max(S0_slice)]
	R1_series->Trim, [0E, 1.0]

    exit: PMI__Control, ev.top, /refresh
end


pro PMI__Button__Control__LinearVfa, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns)
		sensitive = ns gt 4
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__LinearVfa, parent, value=value,separator=separator

	PMI__FitT1SaturationRecovery

    if n_elements(value) eq 0 then value = 'VFA T1-mapping'

    id = widget_button(parent $
    ,   value = value  $
    ,  	event_pro = 'PMI__Button__Event__LinearVfa' $
    ,	pro_set_value = 'PMI__Button__Control__LinearVfa' $
    ,  	separator = separator )

    return, id
end

