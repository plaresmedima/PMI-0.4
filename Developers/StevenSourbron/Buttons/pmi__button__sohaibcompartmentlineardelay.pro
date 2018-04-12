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

FUNCTION PMI__Button__Input__sohaibcompartmentlineardelay, top, series, aif, in

    PMI__Info, top, Stdy=Stdy
    DynSeries = Stdy->Names(0,DefDim=3,ind=ind,sel=sel)
	in = {ser:sel, aif:stdy->sel(1), nb:1}

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
    			Aif = LMU__Enhancement(Aif,in.nb,relative=0)/0.55
				return, 1
    		ENDELSE
    	ENDELSE
     	IF 'Cancel' EQ dialog_message(msg,/information,/cancel) THEN return, 0
  	ENDWHILE
END



pro PMI__Button__Event__sohaibcompartmentlineardelay, ev

	PMI__Info, ev.top, Status=Status, Stdy=Stdy
	PMI__Message, status, 'Preparing calculation..'

    IF NOT PMI__Button__Input__sohaibcompartmentlineardelay(ev.top,series,aif,in) THEN RETURN

	PMI__Message, status, 'Calculating..'

	Dom = {z:Series->z(), t:Series->t(0), m:Series->m()}
    Sbf = Stdy->New('SERIES', Domain= Dom,  Name= 'Blood Flow (ml/100ml/min)' )
    Sev = Stdy->New('SERIES', Domain= Dom,  Name= 'Extracellular Volume (ml/100ml)' )
    Stt = Stdy->New('SERIES', Domain= Dom,  Name= 'Mean Transit Time (sec)' )
    Std = Stdy->New('SERIES', Domain= Dom,  Name= 'Arterial Delay Time (sec)' )

	d = Series->d()
	time = Series->t() - Series->t(0)

ttt=systime(1)

	for j=0L,d[2]-1 do begin

		PMI__Message, status, 'Calculating ', j/(d[2]-1E)

		P = Series->Read(Stdy->DataPath(),z=Series->z(j))
		P = reform(P,d[0]*d[1],d[3],/overwrite)
		if in.nB eq 1 then P0 = reform(P[*,0]) else P0 = total(P[*,0:in.nB-1],2)/in.nB
    	P = P - rebin(P0,d[0]*d[1],d[3])

    	BF = fltarr(d[0],d[1])
    	VE = fltarr(d[0],d[1])
    	TT = fltarr(d[0],d[1])
    	TD = fltarr(d[0],d[1])

		for k=0L,d[0]*d[1]-1 do begin
   			curve = reform(P[k,*])
			FitToftsLinear, time, aif, curve, ve=ecv, Ktrans=Ktrans, DELAY_PAR=delay, DELAY_VALUES=[0,5,0.25]
			BF[k] = 6000E*Ktrans/0.55
			VE[k] = 100E*ecv
			TT[k] = ecv/Ktrans
			TD[k] = delay
		endfor

		Sbf -> Write, Stdy->DataPath(), BF, j
		Sev -> Write, Stdy->DataPath(), VE, j
		Stt -> Write, Stdy->DataPath(), TT, j
		Std -> Write, Stdy->DataPath(), TD, j
	endfor

print, systime(1)-ttt

	Sbf -> Trim, 600, 1
	Sev -> Trim, 100, 1
	Stt -> Trim, 10, 1
	Std -> Trim, 10, 1

    PMI__Control, ev.top, /refresh
end


pro PMI__Button__Control__sohaibcompartmentlineardelay, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 0)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__sohaibcompartmentlineardelay, parent,value=value,separator=separator

    if n_elements(value) eq 0 then value = 'Fermi analysis (Pixel)'

    id = widget_button(parent $
    ,   value = value  $
    ,  	event_pro = 'PMI__Button__Event__sohaibcompartmentlineardelay' $
    ,	pro_set_value = 'PMI__Button__Control__sohaibcompartmentlineardelay' $
    ,  	separator = separator )

    return, id
end

