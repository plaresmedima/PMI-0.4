FUNCTION PMI__Button__Input__ArshadDeconvolution, top, series, aif, in

    PMI__Info, top, Status=Status, Stdy=Stdy
    DynSeries = Stdy->Names(0,DefDim=3,ind=ind,sel=sel)
	in = {ser:sel, aif:stdy->sel(1), nb:2}

	WHILE 1 DO BEGIN

		in = PMI__Form(top, Title='Perfusion analysis setup', [$
		ptr_new({Type:'DROPLIST',Tag:'ser', Label:'Dynamic series', Value:DynSeries, Select:in.ser}), $
		ptr_new({Type:'DROPLIST',Tag:'aif', Label:'Arterial region', Value:Stdy->names(1), Select:in.aif}), $
		ptr_new({Type:'DROPLIST',Tag:'rel', Label:'Inhomogeneity correction?', Value:['No','Yes'], Select:0}), $
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
    			Aif = LMU__Enhancement(Aif,in.nb,relative=in.rel)/0.55
				return, 1
    		ENDELSE
    	ENDELSE
     	IF 'Cancel' EQ dialog_message(msg,/information,/cancel) THEN return, 0
  	ENDWHILE
END



pro PMI__Button__Event__ArshadDeconvolution, ev

	PMI__Info, ev.top, Status=Status, Stdy=Stdy
    IF NOT PMI__Button__Input__ArshadDeconvolution(ev.top,series,aif,in) THEN RETURN

	Dom = {z:Series->z(), t:Series->t(0), m:Series->m()}
    Spf = Stdy->New('SERIES', Domain= Dom, ClrWin=[0E,300E],  Name= Series->name() +'__Blood Flow (ml/100ml/min)' )
    Svd = Stdy->New('SERIES', Domain= Dom, ClrWin=[0E,40E] ,  Name= Series->name() +'__Extracellular Volume (ml/100ml)')
    Stt = Stdy->New('SERIES', Domain= Dom, ClrWin=[0E,30E] ,  Name= Series->name() +'__Mean Transit Time (sec)' )

	d = Series->d()
	time = Series->t() - Series->t(0)

	for j=0L,d[2]-1 do begin

		PMI__Message, status, 'Calculating', j/(d[2]-1E)

		P = Series->Read(Stdy->DataPath(),z=Series->z(j))
		P = reform(P,d[0]*d[1],d[3],/overwrite)
		k = where(P[*,0] gt 0, cnt)

		if cnt gt 0 then begin

			P = P[k, *]

			if in.nB eq 1 then P0 = reform(P[*,0]) else P0 = total(P[*,0:in.nB-1],2)/in.nB
    		P0 = rebin(P0,cnt,d[3])
    		if in.rel then P=P/P0-1 else P=P-P0

			RegridDeconvolutionPixelData,time,P,aif,time_regr=t,aif_regr=a
			InvertIllPosedFast, P, (t[1]-t[0])*convolution_matrix(a), REGPAR=0.15

;	    	INVERTILLPOSED, P, (t[1]-t[0])*convolution_matrix(a), $
;	    		pc='LCC', wm=1L, ws=1L, m0=0.001, m1=1.0, nm=100L

			PF = fltarr(d[0]*d[1]) & PF[k] = max(P,dimension=2)
    		VD = fltarr(d[0]*d[1]) & VD[k] = (t[1]-t[0])*total(P,2)
    		TT = fltarr(d[0]*d[1]) & TT[k] = remove_inf(VD[k]/PF[k])

			Spf->Write, Stdy->DataPath(), 6000*PF/0.55, j
			Svd->Write, Stdy->DataPath(), 100*VD, j
			Stt->Write, Stdy->DataPath(), TT, j
		endif
	endfor

    PMI__Control, ev.top, /refresh
end


pro PMI__Button__Control__ArshadDeconvolution, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 0)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__ArshadDeconvolution, parent,value=value,separator=separator

    if n_elements(value) eq 0 then value = 'Deconvolution analysis (Pixel)'

    id = widget_button(parent $
    ,   value = value  $
    ,  	event_pro = 'PMI__Button__Event__ArshadDeconvolution' $
    ,	pro_set_value = 'PMI__Button__Control__ArshadDeconvolution' $
    ,  	separator = separator )

    return, id
end

