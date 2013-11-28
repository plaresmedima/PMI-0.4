;
;
;    Copyright (C) 2009 Steven Sourbron
;
;    This program is free software; you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation; either version 2 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License along
;    with this program; if not, write to the Free Software Foundation, Inc.,
;    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
;
;
;

FUNCTION PMI__Button__Input__FastDeconvolutionAnalysisPVcorr, top, series, aif, roi, in

    PMI__Info, top, Stdy=Stdy
    DynSeries = Stdy->Names(0,DefDim=3,ind=ind,sel=sel)
	in = {ser:sel, aif:stdy->sel(1), roi:0, vof:0, rel:0, nb:10, hct:0.45}

	WHILE 1 DO BEGIN

		in = PMI__Form(top, Title='Perfusion analysis setup', [$
		ptr_new({Type:'DROPLIST',Tag:'ser', Label:'Dynamic series', Value:DynSeries, Select:in.ser}), $
		ptr_new({Type:'DROPLIST',Tag:'aif', Label:'Arterial Region', Value:Stdy->names(1), Select:in.aif}), $
		ptr_new({Type:'DROPLIST',Tag:'roi', Label:'Tissue Region', Value:['<entire image>',Stdy->names(1)], Select:in.roi}), $
		ptr_new({Type:'DROPLIST',Tag:'vof', Label:'Venous Region', Value:['<none>',Stdy->names(1)], Select:in.vof}), $
		ptr_new({Type:'DROPLIST',Tag:'rel', Label:'Approximate tracer concentrations by:', Value:['Signal Enhancement (T1)','Relative Signal Enhancement (T1)','Relative Signal Enhancement (T2)'], Select:in.rel}), $
		ptr_new({Type:'VALUE'	,Tag:'nb' , Label:'Length of baseline (# of dynamics)', Value:in.nb}),$
		ptr_new({Type:'VALUE'	,Tag:'hct', Label:'Patients hematocrit', Value:in.hct})])
		IF in.cancel THEN return, 0

    	Series = Stdy->Obj(0,ind[in.ser])
    	IF (in.nb LT 1) or (in.nb GT Series->d(3)) THEN BEGIN
    		in.nb = 10
    		msg = ['Baseline length must be less than the total number of dynamics','Please select another baseline length']
    		IF 'Cancel' EQ dialog_message(msg,/information,/cancel) THEN BREAK ELSE CONTINUE
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
    	Aif = LMU__Enhancement(Aif,in.nb,relative=in.rel)/(1-in.hct)
    	IF in.roi GT 0 THEN Roi = Stdy->Obj(1,in.roi-1)
    	IF in.vof EQ 0 THEN return, 1
    	;correct partial volume
    	Vof = PMI__RoiCurve(Stdy->DataPath(), Series, Stdy->Obj(1,in.vof-1), status, cnt=cnt)
        IF cnt EQ 0 THEN BEGIN
    		msg = ['Venous region is empty on this series','Please select another region and/or series']
    		IF 'Cancel' EQ dialog_message(msg,/information,/cancel) THEN BREAK ELSE CONTINUE
    	ENDIF
    	IF n_elements(Vof) NE Series->d(3) THEN BEGIN
    		msg = ['Venous region is not defined on every dynamic','Please select another region and/or series']
    		IF 'Cancel' EQ dialog_message(msg,/information,/cancel) THEN BREAK ELSE CONTINUE
    	ENDIF
    	Vof = LMU__Enhancement(Vof,in.nb,relative=in.rel)
    	time = Series->t()-Series->t(0)
		IRF = DeconvolveCurve(time,	vof, aif, dt=dt, pc='GCV', wm=1L, m0=0.001, m1=1.0, nm=100L, Quad='O2')
		Aif = Aif*dt*total(IRF)
    	return, 1
  	ENDWHILE
  	return, 0
END



pro PMI__Button__Event__FastDeconvolutionAnalysisPVcorr, ev

	PMI__Info, ev.top, Status=Status, Stdy=Stdy
	PMI__Message, status, 'Preparing calculation..'

    IF NOT PMI__Button__Input__FastDeconvolutionAnalysisPVcorr(ev.top,series,aif,roi,in) THEN RETURN

	PMI__Message, status, 'Preparing calculation..'

	Dom = {z:Series->z(), t:Series->t(0), m:Series->m()}
    Spf = Stdy->New('SERIES', Domain= Dom,  Name= 'Plasma Flow (ml/100ml/min)' )
    Svd = Stdy->New('SERIES', Domain= Dom,  Name= 'Volume of Distribution (ml/100ml)')
    Stt = Stdy->New('SERIES', Domain= Dom,  Name= 'Mean Transit Time (sec)' )

	d = Series->d()
	time = Series->t() - Series->t(0)

	for j=0L,d[2]-1 do begin

		PMI__Message, status, 'Calculating ', j/(d[2]-1E)

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
			nozero = where(P0 NE 0, cnt)

			if cnt gt 0 then begin

				P = P[nozero,*]
    			P0 = rebin(P0[nozero],cnt,d[3])
    			case in.rel of
    				0:P = P-P0
    				1:P = P/P0 -1
    				2:P = -alog(P/P0)
    			endcase

				RegridDeconvolutionPixelData,time,P,aif,time_regr=t,aif_regr=a
				InvertIllPosedFast, P, (t[1]-t[0])*convolution_matrix(a), REGPAR=0.15

				PF = fltarr(d[0]*d[1]) & PF[k[nozero]] = 6000*remove_inf(max(P,dimension=2))
    			VD = fltarr(d[0]*d[1]) & VD[k[nozero]] = 100*remove_inf((t[1]-t[0])*total(P,2))
    			TT = fltarr(d[0]*d[1]) & TT[k[nozero]] = 60E*remove_inf(VD[k[nozero]]/PF[k[nozero]])

				Spf->Write, Stdy->DataPath(), PF, j
				Svd->Write, Stdy->DataPath(), VD, j
				Stt->Write, Stdy->DataPath(), TT, j

			endif
		endif
	endfor

	Spf->Trim, [0E, 30E]
	Svd->Trim, [0E, 10E]
	Stt->Trim, [0E, 200E]

    PMI__Control, ev.top, /refresh
end


pro PMI__Button__Control__FastDeconvolutionAnalysisPVcorr, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 0)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__FastDeconvolutionAnalysisPVcorr, parent,value=value,separator=separator

    if n_elements(value) eq 0 then value = 'Fast deconvolution analysis (Pixel)'

    id = widget_button(parent $
    ,   value = value  $
    ,  	event_pro = 'PMI__Button__Event__FastDeconvolutionAnalysisPVcorr' $
    ,	pro_set_value = 'PMI__Button__Control__FastDeconvolutionAnalysisPVcorr' $
    ,  	separator = separator )

    return, id
end

