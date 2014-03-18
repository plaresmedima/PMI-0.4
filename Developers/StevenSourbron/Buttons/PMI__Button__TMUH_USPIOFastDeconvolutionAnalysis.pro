FUNCTION PMI__Button__Input__TMUH_USPIOFastDeconvolutionAnalysis, top, series, aif, in

    PMI__Info, top, Stdy=Stdy
    DynSeries = Stdy->Names(0,DefDim=3,ind=ind,sel=sel)
	in = {ser:sel, aif:stdy->sel(1), nb:10}

	WHILE 1 DO BEGIN

		in = PMI__Form(top, Title='Perfusion analysis setup', [$
		ptr_new({Type:'DROPLIST',Tag:'ser', Label:'Dynamic series', Value:DynSeries, Select:in.ser}), $
		ptr_new({Type:'DROPLIST',Tag:'aif', Label:'Arterial Region', Value:Stdy->names(1), Select:in.aif}), $
		ptr_new({Type:'VALUE'	,Tag:'nb' , Label:'Length of baseline (# of dynamics)', Value:in.nb})])
		IF in.cancel THEN return, 0

    	Series = Stdy->Obj(0,ind[in.ser])
    	IF (in.nb LT 1) or (in.nb GT Series->d(3)) THEN BEGIN
    		in.nb = 10
    		msg = ['Baseline length must be less than the total number of dynamics',$
    		'Please select another baseline length']
    	ENDIF ELSE BEGIN
	   		Aif = PMI__RoiCurve(Stdy->DataPath(), Series, Stdy->Obj(1,in.aif), status, cnt=cnt)
    		IF cnt EQ 0 THEN $
    			msg = ['Arterial region is empty on this series',$
    			'Please select another region and/or series'] $
    		ELSE IF n_elements(Aif) NE Series->d(3) THEN $
    			msg = ['Arterial region is not defined on every dynamic',$
    			'Please select another region and/or series'] $
    		ELSE BEGIN
    			Aif = LMU__Enhancement(Aif,in.nb,relative=2)
				return, 1
    		ENDELSE
    	ENDELSE
     	IF 'Cancel' EQ dialog_message(msg,/information,/cancel) THEN return, 0
  	ENDWHILE
END



pro PMI__Button__Event__TMUH_USPIOFastDeconvolutionAnalysis, ev

	PMI__Info, ev.top, Status=Status, Stdy=Stdy
	PMI__Message, status, 'Preparing calculation..'

    IF NOT PMI__Button__Input__TMUH_USPIOFastDeconvolutionAnalysis(ev.top,series,aif,in) THEN RETURN

	PMI__Message, status, 'Preparing calculation..'

	Dom = {z:Series->z(), t:Series->t(0), m:Series->m()}
    Spf = Stdy->New('SERIES', Domain= Dom,  Name= 'Blood Flow (ml/100ml/min)' )
    Svd = Stdy->New('SERIES', Domain= Dom,  Name= 'Blood Volume (ml/100ml)')
    Stt = Stdy->New('SERIES', Domain= Dom,  Name= 'Mean Transit Time (sec)' )

	d = Series->d()
	time = Series->t() - Series->t(0)

	for j=0L,d[2]-1 do begin

		PMI__Message, status, 'Calculating ', j/(d[2]-1E)

		P = Series->Read(Stdy->DataPath(),z=Series->z(j))
		P = reform(P,d[0]*d[1],d[3],/overwrite)

		if in.nB eq 1 then P0 = reform(P[*,0]) else P0 = total(P[*,0:in.nB-1],2)/in.nB
		nozero = where(P0 NE 0, cnt)

		if cnt gt 0 then begin

			P = P[nozero,*]
    		P0 = rebin(P0[nozero],cnt,d[3])
    		P = -alog(P/P0)

			InvertIllPosedFast, P, time[1]*convolution_matrix(aif), REGPAR=0.15

			PF = fltarr(d[0]*d[1]) & PF[nozero] = 6000*remove_inf(max(P,dimension=2))
    		VD = fltarr(d[0]*d[1]) & VD[nozero] = 100*remove_inf(time[1]*total(P,2))
    		TT = fltarr(d[0]*d[1]) & TT[nozero] = 60E*remove_inf(VD[nozero]/PF[nozero])

			Spf->Write, Stdy->DataPath(), PF, j
			Svd->Write, Stdy->DataPath(), VD, j
			Stt->Write, Stdy->DataPath(), TT, j
		endif

	endfor

	Spf->Trim, 1000, 1
	Svd->Trim, 200, 1
	Stt->Trim, 10, 1

    PMI__Control, ev.top, /refresh
end


pro PMI__Button__Control__TMUH_USPIOFastDeconvolutionAnalysis, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 0)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__TMUH_USPIOFastDeconvolutionAnalysis, parent,value=value,separator=separator

    if n_elements(value) eq 0 then value = 'Fast deconvolution analysis (Pixel)'

    id = widget_button(parent $
    ,   value = value  $
    ,  	event_pro = 'PMI__Button__Event__TMUH_USPIOFastDeconvolutionAnalysis' $
    ,	pro_set_value = 'PMI__Button__Control__TMUH_USPIOFastDeconvolutionAnalysis' $
    ,  	separator = separator )

    return, id
end

