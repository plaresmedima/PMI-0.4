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



FUNCTION PMI__Button__Input__MarcoExtPatlakPixel, top, series, aif, in

    PMI__Info, top, Stdy=Stdy
    DynSeries = Stdy->Names(0,DefDim=3,ind=ind,sel=sel)
	in = {ser:sel, aif:stdy->sel(1), nb:1}

	WHILE 1 DO BEGIN

		in = PMI__Form(top, Title='Perfusion analysis setup', [$
		    ptr_new({Type:'DROPLIST',Tag:'ser', Label:'Dynamic series', Value:DynSeries, Select:in.ser}), $
		    ptr_new({Type:'DROPLIST',Tag:'aif', Label:'Arterial Region', Value:Stdy->names(1), Select:in.aif}), $
		    ptr_new({Type:'VALUE'	,Tag:'nb' , Label:'Length of baseline (# of dynamics)', Value:in.nb})])
		IF in.cancel THEN return, 0

    	IF in.nb LT 1 THEN BEGIN
    		in.nb = 1
    		msg = ['Baseline length must be greater or equal to 1','Please select another baseline length']
    		IF 'Cancel' EQ dialog_message(msg,/information,/cancel) THEN BREAK ELSE CONTINUE
    	ENDIF
    	Series = Stdy->Obj(0,ind[in.ser])
    	IF in.nb GT Series->d(3) THEN BEGIN
    		in.nb = 1
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
    	Aif = LMU__Enhancement(Aif,1,relative=0)/(1-0.45)
    	return, 1

  	ENDWHILE
  	return, 0
END



pro PMI__Button__Event__MarcoExtPatlakPixel, ev

	PMI__Info, ev.top, Status=Status, Stdy=Stdy
	PMI__Message, status, 'Preparing calculation..'

    IF NOT PMI__Button__Input__MarcoExtPatlakPixel(ev.top,series,aif,in) THEN RETURN

	Dom = {z:Series->z(), t:Series->t(0), m:Series->m()}
	Sur = Stdy->New('SERIES', Domain= Dom,  Name= 'Intracellular Uptake Rate (/100/min)')
	Sev = Stdy->New('SERIES', Domain= Dom,  Name= 'Extracellular Volume (ml/100ml)')
    Ske = Stdy->New('SERIES', Domain= Dom,  Name= 'Intracellular Clearance Rate (/100/min)')

	Sur->Trim, 0E, 1
	Sev->Trim, 0E, 1
	Ske->Trim, 0E, 1

    d = Series->d()
	time = Series->t() - Series->t(0)

	URim = fltarr(d[0]*d[1])
	EVim = fltarr(d[0]*d[1])
	KEim = fltarr(d[0]*d[1])

	for j=0L,d[2]-1 do begin

		PMI__Message, status, 'Calculating ', j/(d[2]-1E)

		P = Series->Read(Stdy->DataPath(),z=Series->z(j))
		P = reform(P,d[0]*d[1],d[3],/overwrite)
		if in.nB eq 1 then P0 = reform(P[*,0]) else P0 = total(P[*,0:in.nB-1],2)/in.nB
    	P0 = rebin(P0,d[0]*d[1],d[3])
    	P = P - P0

		for i=0L,d[0]*d[1]-1 do begin
			FitModifiedToftsLinear, time, aif, reform(P[i,*]), vp=vp, ve=ve, Ktrans=Ktrans;, /positivity
			URim[i] = 6000D*Ktrans
			EVim[i] = 100*vp
			KEim[i] = 6000D*Ktrans/ve
		endfor

		URim = remove_inf(URim)
		EVim = remove_inf(EVim)
		KEim = remove_inf(KEim)

		Sur->Write, Stdy->DataPath(), URim, j
		Sev->Write, Stdy->DataPath(), EVim, j
		Ske->Write, Stdy->DataPath(), KEim, j

		Sur->Trim, max([Sur->Trim(1),max(URim)]), 1
		Sev->Trim, max([Sev->Trim(1),max(EVim)]), 1
		Ske->Trim, max([Ske->Trim(1),max(KEim)]), 1

	endfor

    PMI__Control, ev.top, /refresh
end


pro PMI__Button__Control__MarcoExtPatlakPixel, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 1)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__MarcoExtPatlakPixel, parent,value=value,separator=separator

    if n_elements(value) eq 0 then value = 'Extended Patlak (Pixel)'

    id = widget_button(parent $
    ,   value = value  $
    ,  	event_pro = 'PMI__Button__Event__MarcoExtPatlakPixel' $
    ,	pro_set_value = 'PMI__Button__Control__MarcoExtPatlakPixel' $
    ,  	separator = separator )

    return, id
end

