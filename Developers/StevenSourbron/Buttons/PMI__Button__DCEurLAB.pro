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


FUNCTION PMI__Button__Conc__DCEurLAB, Signal, S0, Model, nB

	Case Model of
		0: return, Signal-S0
		1: return, remove_inf((Signal-S0)/S0)
		2: return,  remove_inf(Alog(S0/Signal))
		3: begin
		   TR = 5.05 ;ms
		   FA = 17.0 ;degrees
		   T10 = 1000.0 ;ms
		   cFA = cos(!PI*FA/180.0)
		   sFA = sin(!PI*FA/180.0)
		   E10 = exp(-TR/T10)
		   RE = (Signal-S0)/S0
		   X = RE*(E10-1)
		   R1 = remove_inf(-alog((X+E10*(1-cFA))/(1+(X-1)*cFA))/TR)
	;	   X = (Signal-S0)/(S0*sFA) + (1-m)/(1-m*cFA)
	;      R1 = remove_inf(-alog((1-X)/(1-X*cFA))/TR)
		   return, R1 - total(R1[0:nB-1])/nB
		   end
	Endcase
END


FUNCTION PMI__Button__Input__DCEurLAB, top, Series, aif, in, vof=vof

    PMI__Info, top, Stdy=Stdy
    DynSeries = Stdy->Names(0,DefDim=3,ind=ind,sel=sel)
	in = {ser:sel, aif:stdy->sel(1), vof:0, sig:3, nb:1, hct:0.45}

	WHILE 1 DO BEGIN

		in = PMI__Form(top, Title='Perfusion analysis setup', [$
		ptr_new({Type:'DROPLIST',Tag:'ser', Label:'Dynamic series', Value:DynSeries, Select:in.ser}), $
		ptr_new({Type:'DROPLIST',Tag:'aif', Label:'Arterial Region', Value:Stdy->names(1), Select:in.aif}), $
;		ptr_new({Type:'DROPLIST',Tag:'vof', Label:'Venous Region', Value:['<none>',Stdy->names(1)], Select:in.vof}), $
		ptr_new({Type:'DROPLIST',Tag:'sig', Label:'Signal model:', Value:['Linear (a.u.)','Linear (%)', 'DSC-MRI', 'DCE@urLAB'], Select:in.sig}), $
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
    	;no pv correction allowed
    	return, 1
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



pro PMI__Button__Event__DCEurLAB, ev

	PMI__Info, ev.top, Status=Status, Stdy=Stdy
	PMI__Message, status, 'Preparing calculation..'

    IF NOT PMI__Button__Input__DCEurLAB(ev.top,series,aif,in,vof=vof) THEN RETURN

	Dom = {z:Series->z(), t:Series->t(0), m:Series->m()}
	Sve = Stdy->New('SERIES', Domain= Dom,  Name= 'Interstitial Volume (ml/100ml)')
    Svb = Stdy->New('SERIES', Domain= Dom,  Name= 'Plasma Volume (ml/100ml)' )
    Skt = Stdy->New('SERIES', Domain= Dom,  Name= 'Ktrans (ml/min/100ml)')
    Ske = Stdy->New('SERIES', Domain= Dom,  Name= 'kep (ml/min/100ml)')

	time = Series->t()-Series->t(0)

	Aif = PMI__Button__Conc__DCEurLAB(Aif,total(AIF[0:in.nb-1])/in.nb,in.sig,in.nb)/(1-in.hct)
	if n_elements(vof) ne 0 then begin
    	Vof = PMI__Button__Conc__DCEurLAB(Vof,total(VOF[0:in.nb-1])/in.nb,in.sig,in.nb)/(1-in.hct)
    	IRF = DeconvolveCurve(time, vof, aif, dt=dt, REGPAR=0.1, Quad='O2')
		Aif = Aif*dt*total(IRF)
	endif

	d = Series->d()

	for j=0L,d[2]-1 do begin

		PMI__Message, status, 'Calculating ', j/(d[2]-1E)

		P = Series->Read(Stdy->DataPath(),z=Series->z(j))
		P = reform(P,d[0]*d[1],d[3],/overwrite)
		if in.nB eq 1 then P0 = reform(P[*,0]) else P0 = total(P[*,0:in.nB-1],2)/in.nB

    	P0 = rebin(P0,d[0]*d[1],d[3])
    	P = PMI__Button__Conc__DCEurLAB(P,P0,in.sig,in.nb)

		VBim = fltarr(d[0]*d[1])
		VEim = fltarr(d[0]*d[1])
		KTim = fltarr(d[0]*d[1])
		KEim = fltarr(d[0]*d[1])
		for i=0L,d[0]*d[1]-1 do begin
			FitModifiedToftsLinear, time, aif, reform(P[i,*]), vp=vp, ve=ve, Ktrans=Ktrans;, /positivity
			VBim[i] = 100*vp
			VEim[i] = 100*ve
			KTim[i] = 6000D*Ktrans
			KEim[i] = 6000D*Ktrans/ve
		endfor
		Svb->Write, Stdy->DataPath(), remove_inf(VBim), j
		Sve->Write, Stdy->DataPath(), remove_inf(VEim), j
		Skt->Write, Stdy->DataPath(), remove_inf(KTim), j
		Ske->Write, Stdy->DataPath(), remove_inf(KEim), j
	endfor

	Sve->Trim, [0E, 40E]
	Svb->Trim, [0E, 30E]
	Skt->Trim, [0E, 50E]
	Ske->Trim, [0E, 300E]

    PMI__Control, ev.top, /refresh
end


pro PMI__Button__Control__DCEurLAB, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 0)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__DCEurLAB, parent,value=value,separator=separator

    if n_elements(value) eq 0 then value = 'Modified Tofts (Pixel)'

    id = widget_button(parent $
    ,   value = value  $
    ,  	event_pro = 'PMI__Button__Event__DCEurLAB' $
    ,	pro_set_value = 'PMI__Button__Control__DCEurLAB' $
    ,  	separator = separator )

    return, id
end

