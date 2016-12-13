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



FUNCTION PMI__Button__Input__FitModToftsLinPopAif, top, Series, aif, in

    PMI__Info, top, Stdy=Stdy
    DynSeries = Stdy->Names(0,DefDim=3,ind=ind,sel=sel)
	in = {ser:sel, nb:1, t1:1000E}

	WHILE 1 DO BEGIN

		in = PMI__Form(top, Title='Perfusion analysis setup', [$
		ptr_new({Type:'DROPLIST',Tag:'ser', Label:'Dynamic series', Value:DynSeries, Select:in.ser}), $
		ptr_new({Type:'VALUE'	,Tag:'nb' , Label:'Arterial bolus arrival (# of dynamics)', Value:in.nb}),$
		ptr_new({Type:'VALUE'	,Tag:'t1' , Label:'Tissue T1 (msec)', Value:in.t1})])
		IF in.cancel THEN return, 0

    	Series = Stdy->Obj(0,ind[in.ser])
    	IF (in.nb LT 1) or (in.nb GT Series->d(3)) THEN BEGIN
    	  in.nb = 1
    	  msg = ['Baseline length must be less than the total number of dynamics','Please select another baseline length']
    	  IF 'Cancel' EQ dialog_message(msg,/information,/cancel) THEN BREAK ELSE CONTINUE
    	ENDIF
    	return, 1
  	ENDWHILE
  	return, 0
END



pro PMI__Button__Event__FitModToftsLinPopAif, ev

	PMI__Info, ev.top, Status=Status, Stdy=Stdy
	PMI__Message, status, 'Preparing calculation..'

    IF NOT PMI__Button__Input__FitModToftsLinPopAif(ev.top,series,aif,in) THEN RETURN

	Dom = {z:Series->z(), t:Series->t(0), m:Series->m()}
	Sve = Stdy->New('SERIES', Domain= Dom,  Name= 'Interstitial Volume (ml/100ml)')
    Svb = Stdy->New('SERIES', Domain= Dom,  Name= 'Plasma Volume (ml/100ml)' )
    Skt = Stdy->New('SERIES', Domain= Dom,  Name= 'Ktrans (ml/min/100ml)')
    Ske = Stdy->New('SERIES', Domain= Dom,  Name= 'kep (ml/min/100ml)')

	time = Series->t()-Series->t(0)

	Aif = ParkerAif(Time, baseline=in.nb) ;Plasma concentration in mM

	d = Series->d()

	for j=0L,d[2]-1 do begin

		PMI__Message, status, 'Calculating ', j/(d[2]-1E)

		P = Series->Read(Stdy->DataPath(),z=Series->z(j))
		P = reform(P,d[0]*d[1],d[3],/overwrite)
		if in.nB eq 1 then P0 = reform(P[*,0]) else P0 = total(P[*,0:in.nB-1],2)/in.nB

    	P0 = rebin(P0,d[0]*d[1],d[3])
    	P = remove_inf((P-P0)/P0)/(4.5*in.t1/1000)

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


pro PMI__Button__Control__FitModToftsLinPopAif, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		sensitive = ns gt 0
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__FitModToftsLinPopAif, parent,value=value,separator=separator

    if n_elements(value) eq 0 then value = 'Modified Tofts (Pixel)'

    id = widget_button(parent $
    ,   value = value  $
    ,  	event_pro = 'PMI__Button__Event__FitModToftsLinPopAif' $
    ,	pro_set_value = 'PMI__Button__Control__FitModToftsLinPopAif' $
    ,  	separator = separator )

    return, id
end

