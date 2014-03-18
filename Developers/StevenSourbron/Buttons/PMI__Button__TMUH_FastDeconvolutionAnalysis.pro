FUNCTION PMI__Button__Input__TMUH_FastDeconvolutionAnalysis, top, series, aif, in

    PMI__Info, top, Stdy=Stdy
    DynSeries = Stdy->Names(0,DefDim=3,ind=ind,sel=sel)
	in = {ser:sel, aif:stdy->sel(1), nb:1}

	WHILE 1 DO BEGIN

		in = PMI__Form(top, Title='Perfusion analysis setup', [$
		ptr_new({Type:'DROPLIST',Tag:'ser', Label:'Dynamic series', Value:DynSeries, Select:in.ser}), $
		ptr_new({Type:'DROPLIST',Tag:'aif', Label:'Arterial Region', Value:Stdy->names(1), Select:in.aif})])
		IF in.cancel THEN return, 0

    	Series = Stdy->Obj(0,ind[in.ser])

	   	Aif = PMI__RoiCurve(Stdy->DataPath(), Series, Stdy->Obj(1,in.aif), status, cnt=cnt)
    	IF cnt EQ 0 THEN $
    		msg = ['Arterial region is empty on this series',$
    		'Please select another region and/or series'] $
    	ELSE IF n_elements(Aif) NE Series->d(3) THEN $
    		msg = ['Arterial region is not defined on every dynamic',$
    		'Please select another region and/or series'] $
    	ELSE BEGIN
    		Aif = LMU__Enhancement(Aif,1,relative=0)/(1-0.45)
			return, 1
    	ENDELSE

     	IF 'Cancel' EQ dialog_message(msg,/information,/cancel) THEN return, 0
  	ENDWHILE
END



pro PMI__Button__Event__TMUH_FastDeconvolutionAnalysis, ev

	PMI__Info, ev.top, Status=Status, Stdy=Stdy
	PMI__Message, status, 'Preparing calculation..'

    IF NOT PMI__Button__Input__TMUH_FastDeconvolutionAnalysis(ev.top,series,aif,in) THEN RETURN

	PMI__Message, status, 'Preparing calculation..'

	Dom = {z:Series->z(), t:Series->t(0), m:Series->m()}
    Spf = Stdy->New('SERIES', Domain= Dom,  Name= 'Ktrans (ml/100ml/min)' )
    Svd = Stdy->New('SERIES', Domain= Dom,  Name= 'Distribution volume (ml/100ml)')
    Stt = Stdy->New('SERIES', Domain= Dom,  Name= 'Mean Transit Time (sec)' )


	d = Series->d()
	time = Series->t() - Series->t(0)

	for j=0L,d[2]-1 do begin

		PMI__Message, status, 'Calculating ', j/(d[2]-1E)

		cnt = d[0]*d[1]
		P = Series->Read(Stdy->DataPath(),z=Series->z(j))
		P = reform(P,cnt,d[3],/overwrite)
		k = lindgen(d[0]*d[1])

		if cnt gt 0 then begin

			P0 = reform(P[*,0])
			nozero = where(P0 NE 0, cnt)

			if cnt gt 0 then begin

				P = P[nozero,*]
    			P0 = rebin(P0[nozero],cnt,d[3])
    			P = P-P0

				RegridDeconvolutionPixelData,time,P,aif,time_regr=t,aif_regr=a
				InvertIllPosedFast, P, (t[1]-t[0])*convolution_matrix(a), REGPAR=0.1

				PF = fltarr(d[0]*d[1]) & PF[k[nozero]] = 6000*remove_inf(max(P,dimension=2))
    			VD = fltarr(d[0]*d[1]) & VD[k[nozero]] = 100*remove_inf((t[1]-t[0])*total(P,2))
    			TT = fltarr(d[0]*d[1]) & TT[k[nozero]] = 60E*remove_inf(VD[k[nozero]]/PF[k[nozero]])

				Spf->Write, Stdy->DataPath(), PF, j
				Svd->Write, Stdy->DataPath(), VD, j
				Stt->Write, Stdy->DataPath(), TT, j
			endif
		endif
	endfor

	Spf->Trim, [0E, 30]
	Svd->Trim, [0E, 50]
	Stt->Trim, [0E, 300E]

    PMI__Control, ev.top, /refresh
end

pro PMI__Button__Control__FastDeconvolutionAnalysis, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 0)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__TMUH_FastDeconvolutionAnalysis, parent,value=value,separator=separator

    if n_elements(value) eq 0 then value = 'Fast deconvolution analysis (Pixel)'

    id = widget_button(parent $
    ,   value = value  $
    ,  	event_pro = 'PMI__Button__Event__TMUH_FastDeconvolutionAnalysis' $
    ,	pro_set_value = 'PMI__Button__Control__FastDeconvolutionAnalysis' $
    ,  	separator = separator )

    return, id
end

