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

FUNCTION PMI__Button__Input__Sergios_FastDeconvolutionAnalysis, top, series, aif, in

    PMI__Info, top, Stdy=Stdy
    DynSeries = Stdy->Names(0,DefDim=3,ind=ind,sel=sel)
	in = {ser:sel, aif:stdy->sel(1), rel:1, nt:5.0, hct:0.45}

	WHILE 1 DO BEGIN

		in = PMI__Form(top, Title='Perfusion analysis setup', [$
		ptr_new({Type:'DROPLIST',Tag:'ser', Label:'Dynamic series', Value:DynSeries, Select:in.ser}), $
		ptr_new({Type:'DROPLIST',Tag:'aif', Label:'Arterial Region', Value:Stdy->names(1), Select:in.aif}), $
		ptr_new({Type:'DROPLIST',Tag:'rel', Label:'Signal model', Value:['Linear (a.u.)','Linear (%)'], Select:in.rel}), $
		ptr_new({Type:'VALUE'	,Tag:'nt' , Label:'Length of baseline (sec)', Value:in.nt}),$
		ptr_new({Type:'VALUE'	,Tag:'hct', Label:'Patients hematocrit', Value:in.hct})])
		IF in.cancel THEN return, 0

        Series = Stdy->Obj(0,ind[in.ser])
        time = Series->t() - Series->t(0)
  		nb = ceil(in.nt/time[1])

    	IF (nb LT 1) or (nb GT Series->d(3)) THEN BEGIN
    		in.nt = 5.0
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
    			Aif = LMU__Enhancement(Aif,nb,relative=in.rel)/(1-in.hct)
				return, 1
    		ENDELSE
    	ENDELSE
     	IF 'Cancel' EQ dialog_message(msg,/information,/cancel) THEN return, 0
  	ENDWHILE
END



pro PMI__Button__Event__Sergios_FastDeconvolutionAnalysis, ev

	PMI__Info, ev.top, Status=Status, Stdy=Stdy
	PMI__Message, status, 'Preparing calculation..'

    IF NOT PMI__Button__Input__Sergios_FastDeconvolutionAnalysis(ev.top,series,aif,in) THEN RETURN

	PMI__Message, status, 'Preparing calculation..'

	Dom = {z:Series->z(), t:Series->t(0), m:Series->m()}
    Spf = Stdy->New('SERIES', Domain= Dom,  Name= 'Plasma Flow (ml/100ml/min)' )
    Svd = Stdy->New('SERIES', Domain= Dom,  Name= 'Extracellular Volume (ml/100ml)')
    Stt = Stdy->New('SERIES', Domain= Dom,  Name= 'Extracellular MTT (sec)' )

	d = Series->d()
	time = Series->t() - Series->t(0)
	nb = ceil(in.nt/time[1])

	for j=0L,d[2]-1 do begin

		PMI__Message, status, 'Calculating ', j/(d[2]-1E)

		P = Series->Read(Stdy->DataPath(),z=Series->z(j))
		P = reform(P,d[0]*d[1],d[3],/overwrite)
		k = lindgen(d[0]*d[1])

		if nB eq 1 then P0 = reform(P[*,0]) else P0 = total(P[*,0:nB-1],2)/nB
		nozero = where(P0 NE 0, cnt)

		if cnt gt 0 then begin

			P = P[nozero,*]
    		P0 = rebin(P0[nozero],cnt,d[3])
    		case in.rel of
    		  0:P = P-P0
    		  1:P = P/P0 -1
    		endcase

			InvertIllPosedFast, P, (time[1]-time[0])*convolution_matrix(aif), REGPAR=0.15

			PF = fltarr(d[0]*d[1]) & PF[k[nozero]] = 6000*remove_inf(max(P,dimension=2))
    		VD = fltarr(d[0]*d[1]) & VD[k[nozero]] = 100*remove_inf((time[1]-time[0])*total(P,2))
    		TT = fltarr(d[0]*d[1]) & TT[k[nozero]] = 60E*remove_inf(VD[k[nozero]]/PF[k[nozero]])

			Spf->Write, Stdy->DataPath(), PF, j
			Svd->Write, Stdy->DataPath(), VD, j
			Stt->Write, Stdy->DataPath(), TT, j

		endif

	endfor

	Spf->Trim, 200, 1
	Svd->Trim, 100, 1
	Stt->Trim, 100, 1

    PMI__Control, ev.top, /refresh
end


pro PMI__Button__Control__Sergios_FastDeconvolutionAnalysis, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 0)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__Sergios_FastDeconvolutionAnalysis, parent,value=value,separator=separator

    if n_elements(value) eq 0 then value = 'Fast deconvolution analysis (Pixel)'

    id = widget_button(parent $
    ,   value = value  $
    ,  	event_pro = 'PMI__Button__Event__Sergios_FastDeconvolutionAnalysis' $
    ,	pro_set_value = 'PMI__Button__Control__Sergios_FastDeconvolutionAnalysis' $
    ,  	separator = separator )

    return, id
end

