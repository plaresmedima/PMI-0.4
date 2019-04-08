FUNCTION PMI__Button__Input__TRISTAN_LL_T1mapping, top, series

    PMI__Info, top, Stdy=Stdy
    DynSeries = Stdy->Names(0,DefDim=3,ind=ind,sel=sel)

	in = PMI__Form(top, Title='T1 mapping input', [$
		ptr_new({Type:'DROPLIST',Tag:'ser', Label:'T1-MOLLI', Value:DynSeries, Select:sel})])
	IF in.cancel THEN return, 0
    Series = Stdy->Obj(0,ind[in.ser])
    return, 1
END



pro PMI__Button__Event__TRISTAN_LL_T1mapping, ev

	PMI__Message, status, 'Preparing calculation..'

    IF NOT PMI__Button__Input__TRISTAN_LL_T1mapping(ev.top,series) THEN goto, exit

	PMI__Info, ev.top, Status=Status, Stdy=Stdy

	Dom = {z:Series->z(), t:Series->t(0), m:Series->m()}
    Sinf = Stdy->New('SERIES', Domain= Dom,  Name= Series->name() + '_Sinf' )
    St1 = Stdy->New('SERIES', Domain= Dom,  Name= Series->name() + '_T1')

	d = Series->d()
	time = Series->t()
	ExpectedT1 = max(time)/2.0

	for j=0L,d[2]-1 do begin

		PMI__Message, status, 'Calculating ', j/(d[2]-1E)

		P = Series->Read(Stdy->DataPath(),z=Series->z(j))
		P = reform(P,d[0]*d[1],d[3],/overwrite)

		Sinf_slice = fltarr(d[0]*d[1])
		T1_slice = fltarr(d[0]*d[1])

		for i=0L,d[0]*d[1]-1 do begin

			Sig = reform(P[i,*])

			Pars = [max(Sig),1/ExpectedT1] ;[Sinf, R1]
			Fit = mpcurvefit(Time, Sig, 1+0E*Sig, Pars, function_name='PMI__FitT1SaturationRecovery',/quiet,NODERIVATIVE=0)
			Sinf_slice[i] = Pars[0]
			T1_slice[i] = 1/Pars[1]
		endfor

		Sinf->Write, Stdy->DataPath(), Sinf_slice, j
		St1->Write, Stdy->DataPath(), T1_slice, j

	endfor

	Sinf->Trim, [0E, max(P)]
	St1->Trim, [0E, max(time)]

    exit: PMI__Control, ev.top, /refresh
end


pro PMI__Button__Control__TRISTAN_LL_T1mapping, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		sensitive = ns gt 0
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__TRISTAN_LL_T1mapping, parent, value=value,separator=separator

	PMI__FitT1SaturationRecovery

    if n_elements(value) eq 0 then value = 'T2 mapping'

    id = widget_button(parent $
    ,   value = value  $
    ,  	event_pro = 'PMI__Button__Event__TRISTAN_LL_T1mapping' $
    ,	pro_set_value = 'PMI__Button__Control__TRISTAN_LL_T1mapping' $
    ,  	separator = separator )

    return, id
end

