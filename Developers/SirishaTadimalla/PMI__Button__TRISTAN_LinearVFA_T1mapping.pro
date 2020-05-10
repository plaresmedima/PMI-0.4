
pro PMI__Button__Event__TRISTAN_LinearVFA_T1mapping, ev

	PMI__Info, ev.top, Stdy=Stdy, Status=Status
	PMI__Message, status, 'Getting user input..'

    SeriesNames = Stdy->Names(0)

	in = PMI__Form(ev.top, Title='VFA T1-mapping input', [$
		ptr_new({Type:'LIST',Tag:'VFA', Label:'Select VFA series', Value:SeriesNames, Select:0})])
	IF in.cancel THEN goto, exit

	sel = in.VFA
	nFA = n_elements(sel)

	Default = Stdy->Obj(0,sel[0])

	Dom = {z:Default->z(), t:Default->t(0), m:Default->m()}
    S0_series = Stdy->New('SERIES', Domain= Dom,  Name= 'VFA_S0')
    R1_series = Stdy->New('SERIES', Domain= Dom,  Name= 'VFA_R1 (ms-1)')
    T1_series = Stdy->New('SERIES', Domain= Dom,  Name= 'VFA_T1 (ms)')
    FIT_series = Stdy->New('SERIES', Domain= Dom,  Name= 'VFA_RMS (%)')

	TR = Default->GETVALUE('0018'x,'0080'x) ;msec
	;print, TR
	;TR = 3.71;0999999

	FA = fltarr(nFA)
	For k=0L, nFA-1 do FA[k] = (Stdy->Obj(0,sel[k]))->GETVALUE('0018'x,'1314'x)

    print, FA

	; if the FAs in the header are all the same, then we need to get them from the user
	if (FA[0] EQ FA[2]) then begin
		if nFA EQ 13 then begin
			v = PMI__Form(ev.top, Title='Select series', [$
			ptr_new({Type:'VALUE', Tag:'fa0', Label:'FA1'	, Value:FA[0]}),$
			ptr_new({Type:'VALUE', Tag:'fa1', Label:'FA2'	, Value:FA[1]}),$
			ptr_new({Type:'VALUE', Tag:'fa2', Label:'FA3'	, Value:FA[2]}),$
			ptr_new({Type:'VALUE', Tag:'fa3', Label:'FA4'	, Value:FA[3]}),$
			ptr_new({Type:'VALUE', Tag:'fa4', Label:'FA5'	, Value:FA[4]}),$
			ptr_new({Type:'VALUE', Tag:'fa5', Label:'FA6'	, Value:FA[5]}),$
			ptr_new({Type:'VALUE', Tag:'fa6', Label:'FA7'	, Value:FA[6]}),$
			ptr_new({Type:'VALUE', Tag:'fa7', Label:'FA8'	, Value:FA[7]}),$
			ptr_new({Type:'VALUE', Tag:'fa8', Label:'FA9'	, Value:FA[8]}),$
			ptr_new({Type:'VALUE', Tag:'fa9', Label:'FA10'	, Value:FA[9]}),$
			ptr_new({Type:'VALUE', Tag:'fa10', Label:'FA11'	, Value:FA[10]}),$
			ptr_new({Type:'VALUE', Tag:'fa11', Label:'FA12'	, Value:FA[11]}),$
			ptr_new({Type:'VALUE', Tag:'fa12', Label:'FA13' , Value:FA[12]})])
			if v.cancel then return

			FA = [v.fa0,v.fa1,v.fa2,v.fa3,v.fa4,v.fa5,v.fa6,v.fa7,v.fa8,v.fa9,v.fa10,v.fa11,v.fa12]
		endif

		if nFA EQ 7 then begin
			v = PMI__Form(ev.top, Title='Select series', [$
			ptr_new({Type:'VALUE', Tag:'fa0', Label:'FA1'	, Value:FA[0]}),$
			ptr_new({Type:'VALUE', Tag:'fa1', Label:'FA2'	, Value:FA[1]}),$
			ptr_new({Type:'VALUE', Tag:'fa2', Label:'FA3'	, Value:FA[2]}),$
			ptr_new({Type:'VALUE', Tag:'fa3', Label:'FA4'	, Value:FA[3]}),$
			ptr_new({Type:'VALUE', Tag:'fa4', Label:'FA5'	, Value:FA[4]}),$
			ptr_new({Type:'VALUE', Tag:'fa5', Label:'FA6'	, Value:FA[5]}),$
			ptr_new({Type:'VALUE', Tag:'fa6', Label:'FA7' , Value:FA[6]})])
			if v.cancel then return

			FA = [v.fa0,v.fa1,v.fa2,v.fa3,v.fa4,v.fa5,v.fa6]
		endif

		if nFA EQ 6 then begin
			v = PMI__Form(ev.top, Title='Select series', [$
			ptr_new({Type:'VALUE', Tag:'fa0', Label:'FA1'	, Value:FA[0]}),$
			ptr_new({Type:'VALUE', Tag:'fa1', Label:'FA2'	, Value:FA[1]}),$
			ptr_new({Type:'VALUE', Tag:'fa2', Label:'FA3'	, Value:FA[2]}),$
			ptr_new({Type:'VALUE', Tag:'fa3', Label:'FA4'	, Value:FA[3]}),$
			ptr_new({Type:'VALUE', Tag:'fa4', Label:'FA5'	, Value:FA[4]}),$
			ptr_new({Type:'VALUE', Tag:'fa5', Label:'FA6' , Value:FA[5]})])
			if v.cancel then return

			FA = [v.fa0,v.fa1,v.fa2,v.fa3,v.fa4,v.fa5]
		endif

		if nFA EQ 4 then begin
			v = PMI__Form(ev.top, Title='Select series', [$
			ptr_new({Type:'VALUE', Tag:'fa0', Label:'FA1'	, Value:FA[0]}),$
			ptr_new({Type:'VALUE', Tag:'fa1', Label:'FA2'	, Value:FA[1]}),$
			ptr_new({Type:'VALUE', Tag:'fa2', Label:'FA3'	, Value:FA[2]}),$
			ptr_new({Type:'VALUE', Tag:'fa3', Label:'FA4' , Value:FA[5]})])
			if v.cancel then return

			FA = [v.fa0,v.fa1,v.fa2,v.fa3]
		endif
	endif
	print, FA
	d = Default->d()
	SVA_slice = fltarr(nFA,d[0]*d[1])
	S0_slice = fltarr(d[0]*d[1])
	R1_slice = fltarr(d[0]*d[1])
	T1_slice = fltarr(d[0]*d[1])
	FIT_slice = fltarr(d[0]*d[1])

	for j=0L,d[2]-1 do begin ;loop over slices

		PMI__Message, status, 'Calculating ', j/(d[2]-1E)

		for k=0L,nFA-1 do SVA_slice[k,*] = (Stdy->Obj(0,sel[k]))->Read(Stdy->DataPath(),z=Default->z(j))

		for i=0L,d[0]*d[1]-1 do begin

			PAR = VFA_Linear_T1fit(TR, FA, reform(SVA_slice[*,i]), RMS = rms)

			S0_slice[i] = PAR[1]
			R1_slice[i] = Par[0]
			T1_slice[i] = 1/Par[0]
			Fit_slice[i] = rms

		endfor

		S0_series->Write, Stdy->DataPath(), S0_slice, j
		R1_series->Write, Stdy->DataPath(), R1_slice, j
		T1_series->Write, Stdy->DataPath(), T1_slice, j
		FIT_series->Write, Stdy->DataPath(), Fit_slice, j

	endfor

	S0_series->Trim, [0E, max(S0_slice,/NAN)]
	R1_series->Trim, [0E, 0.002]
	T1_series->Trim, [0E, 2000.0]
	FIT_series->Trim, [0E, 50.0]

    exit: PMI__Control, ev.top, /refresh
end


pro PMI__Button__Control__TRISTAN_LinearVFA_T1mapping, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns)
		sensitive = ns gt 4
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__TRISTAN_LinearVFA_T1mapping, parent, value=value,separator=separator

    if n_elements(value) eq 0 then value = 'VFA T1-mapping'

    id = widget_button(parent $
    ,   value = value  $
    ,  	event_pro = 'PMI__Button__Event__TRISTAN_LinearVFA_T1mapping' $
    ,	pro_set_value = 'PMI__Button__Control__TRISTAN_LinearVFA_T1mapping' $
    ,  	separator = separator )

    return, id
end

