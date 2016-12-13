FUNCTION PMI__Button__Input__FastDeconvolutionAnalysisStdAif, top, series, aif, roi, in

    PMI__Info, top, Status=Status, Stdy=Stdy
    DynSeries = Stdy->Names(0,DefDim=3,ind=ind,sel=sel)
	in = {ser:sel, roi:0, nb:10, hct:0.45, par:0.15}

	WHILE 1 DO BEGIN

		in = PMI__Form(top, Title='Perfusion analysis setup', [$
		ptr_new({Type:'DROPLIST',Tag:'ser', Label:'Dynamic series', Value:DynSeries, Select:in.ser}), $
		ptr_new({Type:'DROPLIST',Tag:'roi', Label:'Tissue Region', Value:['<entire image>',Stdy->names(1)], Select:in.roi}), $
		ptr_new({Type:'VALUE'	,Tag:'nb' , Label:'Length of baseline (# of dynamics)', Value:in.nb}),$
		ptr_new({Type:'VALUE'	,Tag:'T1' , Label:'Tissue T1 (msec)', Value:700E}),$
		ptr_new({Type:'VALUE'	,Tag:'par', Label:'ADVANCED OPTION: Regularization parameter', Value:in.par})])
		IF in.cancel THEN return, 0

    	Series = Stdy->Obj(0,ind[in.ser])
    	IF (in.nb LT 1) or (in.nb GT Series->d(3)) THEN BEGIN
    		in.nb = 10
    		msg = ['Baseline length must be less than the total number of dynamics',$
    		'Please select another baseline length']
    	ENDIF ELSE BEGIN
	   		Aif = ParkerAif(Series->t()- Series->t(0)) ;Plasma concentration in mM
    		Aif = 4.5 * (in.T1/1000E) * Aif ;Correction for tissue T1 (700ms, kidney) and relaxivity (4.5 Hz/mM)
			if in.roi gt 0 then Roi = Stdy->Obj(1,in.roi-1)
			return, 1
    	ENDELSE
     	IF 'Cancel' EQ dialog_message(msg,/information,/cancel) THEN return, 0
  	ENDWHILE
END



pro PMI__Button__Event__FastDeconvolutionAnalysisStdAif, ev

	PMI__Info, ev.top, Status=Status, Stdy=Stdy
    IF NOT PMI__Button__Input__FastDeconvolutionAnalysisStdAif(ev.top,series,aif,roi,in) THEN RETURN

	Dom = {z:Series->z(), t:Series->t(0), m:Series->m()}
    Spf = Stdy->New('SERIES', Domain= Dom,  Name= 'Fast deconvolution - Plasma Flow (ml/100ml/min)' )
    Svd = Stdy->New('SERIES', Domain= Dom,  Name= 'Fast deconvolution - Volume of Distribution (ml/100ml)')
    Stt = Stdy->New('SERIES', Domain= Dom,  Name= 'Fast deconvolution - Mean Transit Time (sec)' )

	d = Series->d()
	time = Series->t() - Series->t(0)

	for j=0L,d[2]-1 do begin

		PMI__Message, status, 'Calculating', j/(d[2]-1E)

		if obj_valid(Roi) then $
			P = PMI__PixelCurve(Stdy->DataPath(),Series,Roi,z=Series->z(j),cnt=cnt,ind=k) $
		else begin
			cnt = d[0]*d[1]
			P = Series->Read(Stdy->DataPath(),z=Series->z(j))
			P = reform(P,cnt,d[3],/overwrite)
			k = lindgen(d[0]*d[1])
		endelse

		if cnt gt 0 then begin

			if in.nB eq 1 then P0 = reform(P[*,0]) else P0 = total(P[*,0:in.nB-1],2)/in.nB
    		P0 = rebin(P0,cnt,d[3])
    		P = P/P0-1

			RegridDeconvolutionPixelData,time,P,aif,time_regr=t,aif_regr=a
			InvertIllPosedFast, P, (t[1]-t[0])*convolution_matrix(a), REGPAR=in.par

			PF = fltarr(d[0]*d[1]) & PF[k] = max(P,dimension=2)
    		VD = fltarr(d[0]*d[1]) & VD[k] = (t[1]-t[0])*total(P,2)
    		TT = fltarr(d[0]*d[1]) & TT[k] = remove_inf(VD[k]/PF[k])

			Spf->Write, Stdy->DataPath(), 6000*PF, j
			Svd->Write, Stdy->DataPath(), 100*VD, j
			Stt->Write, Stdy->DataPath(), TT, j
		endif
	endfor

    PMI__Control, ev.top, /refresh
end


pro PMI__Button__Control__FastDeconvolutionAnalysisStdAif, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		sensitive = ns gt 0
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__FastDeconvolutionAnalysisStdAif, parent,value=value,separator=separator

    if n_elements(value) eq 0 then value = 'Fast deconvolution analysis with standard AIF (Pixel)'

    id = widget_button(parent $
    ,   value = value  $
    ,  	event_pro = 'PMI__Button__Event__FastDeconvolutionAnalysisStdAif' $
    ,	pro_set_value = 'PMI__Button__Control__FastDeconvolutionAnalysisStdAif' $
    ,  	separator = separator )

    return, id
end

