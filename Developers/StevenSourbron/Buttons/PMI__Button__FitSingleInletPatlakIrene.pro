FUNCTION PMI__Button__Conc__FitSingleInletPatlakIrene, Signal, S0, Model, T1=T1, TR=TR, FA=FA

	If Model eq 0 then return, Signal-S0
	If Model eq 1 then return, (Signal-S0)/S0

	relaxivity = 3.2

	if Model eq 2 then return, (1/(relaxivity*T1))*(Signal-S0)/S0

	return, Concentration_SPGRESS(Signal, S0, T1*1000, FA, TR, relaxivity)
END


FUNCTION PMI__Button__Input__FitSingleInletPatlakIrene, top, Series, aif, in, vof=vof, SatRec=SatRec

    PMI__Info, top, Stdy=Stdy
    DynSeries = Stdy->Names(0,DefDim=3,ind=ind,sel=sel)
	in = {ser:sel, sar:Stdy->sel(0), aif:stdy->sel(1), vof:0, sig:0, nb:10, hct:0.45}

	WHILE 1 DO BEGIN

		in = PMI__Form(top, Title='Perfusion analysis setup', [$
		ptr_new({Type:'DROPLIST',Tag:'ser', Label:'Dynamic series', Value:DynSeries, Select:in.ser}), $
		ptr_new({Type:'DROPLIST',Tag:'aif', Label:'Arterial Region', Value:Stdy->names(1), Select:in.aif}), $
		ptr_new({Type:'DROPLIST',Tag:'vof', Label:'Venous Region', Value:['<none>',Stdy->names(1)], Select:in.vof}), $
		ptr_new({Type:'DROPLIST',Tag:'sig', Label:'Signal model:', Value:['Linear (a.u.)','Linear (%)','Linear (mM)', 'Non-linear (mM)'], Select:in.sig}), $
		ptr_new({Type:'DROPLIST',Tag:'sar', Label:'T1 map (sec)', Value:Stdy->names(0), Select:in.sar}), $
		ptr_new({Type:'VALUE'	,Tag:'nb' , Label:'Length of baseline (# of dynamics)', Value:in.nb}),$
		ptr_new({Type:'VALUE'	,Tag:'hct', Label:'Patients hematocrit', Value:in.hct})])
		IF in.cancel THEN return, 0

    	Series = Stdy->Obj(0,ind[in.ser])
    	IF (in.nb LT 1) or (in.nb GT Series->d(3)) THEN BEGIN
    		in.nb = 10
    		msg = ['Baseline length must be less than the total number of dynamics','Please select another baseline length']
    		IF 'Cancel' EQ dialog_message(msg,/information,/cancel) THEN BREAK ELSE CONTINUE
    	ENDIF
    	SatRec = Stdy->Obj(0,in.sar)
    	IF in.sig GE 2 THEN BEGIN
    		dDyn = Series->d() & dT1 = SatRec->d()
    		if (dDyn[0] ne dT1[0]) or (dDyn[1] ne dT1[1]) or (dDyn[2] ne dT1[2]) then begin
    			msg = ['T1 map must have the same dimensions as dynamic series','Please select the correct T1 map, or create it first']
    			IF 'Cancel' EQ dialog_message(msg,/information,/cancel) THEN BREAK ELSE CONTINUE
    		endif
    	ENDIF
	   	Aif = PMI__RoiCurve(Stdy->DataPath(), Series, Stdy->Obj(1,in.aif), status, cnt=cnt)
    	IF cnt EQ 0 THEN BEGIN
    		msg = ['Arterial region is empty on this series','Please select another region and/or series']
    		IF 'Cancel' EQ dialog_message(msg,/information,/cancel) THEN BREAK ELSE CONTINUE
    	ENDIF
    	IF n_elements(Aif) NE Series->d(3) THEN BEGIN
    		msg = ['Arterial region is not defined on every dynamic','Please select another region and/or series']
    		IF 'Cancel' EQ dialog_message(msg,/information,/cancel) THEN BREAK ELSE CONTINUE
    	ENDIF
    	IF in.vof EQ 0 THEN return, 1
    	Vof = PMI__RoiCurve(Stdy->DataPath(), Series, Stdy->Obj(1,in.vof-1), status, cnt=cnt)
        IF cnt EQ 0 THEN BEGIN
    		msg = ['Venous region is empty on this series','Please select another region and/or series']
    		IF 'Cancel' EQ dialog_message(msg,/information,/cancel) THEN BREAK ELSE CONTINUE
    	ENDIF
    	IF n_elements(Vof) NE Series->d(3) THEN BEGIN
    		msg = ['Venous region is not defined on every dynamic','Please select another region and/or series']
    		IF 'Cancel' EQ dialog_message(msg,/information,/cancel) THEN BREAK ELSE CONTINUE
    	ENDIF
    	return, 1
  	ENDWHILE
  	return, 0
END



pro PMI__Button__Event__FitSingleInletPatlakIrene, ev

	PMI__Info, ev.top, Status=Status, Stdy=Stdy

	PMI__Message, status, 'Preparing calculation..'

    IF NOT PMI__Button__Input__FitSingleInletPatlakIrene(ev.top,series,aif,in,vof=vof,SatRec=SatRec) THEN RETURN

	Dom = {z:Series->z(), t:Series->t(0), m:Series->m()}
	Spf = Stdy->New('SERIES', Domain= Dom,  Name= 'Blood Flow (ml/100ml/min)' )
	Svd = Stdy->New('SERIES', Domain= Dom,  Name= 'Volume of Distribution (ml/100ml)')
	Stt = Stdy->New('SERIES', Domain= Dom,  Name= 'Mean Transit Time (sec)' )
    Svp = Stdy->New('SERIES', Domain= Dom,  Name= 'Blood Volume (ml/100ml)' )
    Sps = Stdy->New('SERIES', Domain= Dom,  Name= 'Ktrans (ml/min/100ml)')

	time = Series->t()-Series->t(0)
	TR = series->GETVALUE('0018'x,'0080'x)
	FA = series->GETVALUE('0018'x,'1314'x)

	Aif = PMI__Button__Conc__FitSingleInletPatlakIrene(Aif,total(AIF[0:in.nb-1])/in.nb,in.sig,T1=1.932, TR=TR, FA=FA)/(1-in.hct)
	if n_elements(vof) ne 0 then begin
    	Vof = PMI__Button__Conc__FitSingleInletPatlakIrene(Vof,total(VOF[0:in.nb-1])/in.nb,in.sig,T1=1.932, TR=TR, FA=FA)/(1-in.hct)
		IRF = DeconvolveCurve(time,	vof, aif, dt=dt, pc='GCV', wm=1L, m0=0.001, m1=1.0, nm=100L, Quad='O2')
		Aif = Aif*dt*total(IRF)
	endif

	d = Series->d()

	for j=0L,d[2]-1 do begin

		PMI__Message, status, 'Calculating ', j/(d[2]-1E)

		P = Series->Read(Stdy->DataPath(),z=Series->z(j))
		P = reform(P,d[0]*d[1],d[3],/overwrite)
		if in.nB eq 1 then P0 = reform(P[*,0]) else P0 = total(P[*,0:in.nB-1],2)/in.nB
		if in.sig ge 2 then T1 = reform(SatRec->Read(Stdy->DataPath(),z=SatRec->z(j)),d[0]*d[1]) else T1=P0

    	P0 = rebin(P0,d[0]*d[1],d[3])
    	T1 = rebin(T1,d[0]*d[1],d[3])
    	P = PMI__Button__Conc__FitSingleInletPatlakIrene(P,P0,in.sig,T1=T1, TR=TR, FA=FA)

		VP = fltarr(d[0]*d[1])
		PS = fltarr(d[0]*d[1])
		for i=0L,d[0]*d[1]-1 do begin
			Fit = FitPatlak(time, reform(P[i,*]), aif, Interval=[time[in.nB],max(time)], Pars=Pars)
			VP[i] = 100*Pars[0]/(1-in.hct)
			PS[i] = 6000D*Pars[1]
		endfor
		Svp->Write, Stdy->DataPath(), remove_inf(VP), j
		Sps->Write, Stdy->DataPath(), remove_inf(PS), j

		InvertIllPosedFast, P, time[1]*convolution_matrix(aif), REGPAR=0.15

		PF = fltarr(d[0]*d[1])
    	VD = fltarr(d[0]*d[1])
    	TT = fltarr(d[0]*d[1])

		PF = 6000*remove_inf(max(P,dimension=2))/(1-in.hct)
		VD = 100*remove_inf(time[1]*total(P,2))
		TT = 60*remove_inf(VD/PF)

		Spf->Write, Stdy->DataPath(), PF, j
		Svd->Write, Stdy->DataPath(), VD, j
		Stt->Write, Stdy->DataPath(), TT, j

	endfor

	Spf->Trim, [0E, 60E]
	Svd->Trim, [0E, 20E]
	Stt->Trim, [0E, 60E]
	Svp->Trim, [0E, 30E]
	Sps->Trim, [0E, 10E]

    PMI__Control, ev.top, /refresh
end


pro PMI__Button__Control__FitSingleInletPatlakIrene, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 0)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__FitSingleInletPatlakIrene, parent,value=value,separator=separator

    if n_elements(value) eq 0 then value = 'Fast Patlak analysis (Pixel)'

    id = widget_button(parent $
    ,   value = value  $
    ,  	event_pro = 'PMI__Button__Event__FitSingleInletPatlakIrene' $
    ,	pro_set_value = 'PMI__Button__Control__FitSingleInletPatlakIrene' $
    ,  	separator = separator )

    return, id
end

