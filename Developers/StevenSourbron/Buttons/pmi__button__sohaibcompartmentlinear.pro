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

FUNCTION PMI__Button__Input__sohaibcompartmentlinear, top, series, aif, roi, in

    PMI__Info, top, Stdy=Stdy
    DynSeries = Stdy->Names(0,DefDim=3,ind=ind,sel=sel)
	in = {ser:sel, aif:stdy->sel(1), roi:0L, nb:1}

	WHILE 1 DO BEGIN

		in = PMI__Form(top, Title='Perfusion analysis setup', [$
		ptr_new({Type:'DROPLIST',Tag:'ser', Label:'Dynamic series', Value:DynSeries, Select:in.ser}), $
		ptr_new({Type:'DROPLIST',Tag:'aif', Label:'Arterial Region', Value:Stdy->names(1), Select:in.aif}), $
		ptr_new({Type:'DROPLIST',Tag:'roi', Label:'Window', Value:['<entire image>',Stdy->names(1)], Select:in.roi}), $
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
    			if in.roi gt 0 then Roi = Stdy->Obj(1,in.roi-1)
				return, 1
    		ENDELSE
    	ENDELSE
     	IF 'Cancel' EQ dialog_message(msg,/information,/cancel) THEN return, 0
  	ENDWHILE
END



pro PMI__Button__Event__sohaibcompartmentlinear, ev

	PMI__Info, ev.top, Status=Status, Stdy=Stdy
	PMI__Message, status, 'Preparing calculation..'

    IF NOT PMI__Button__Input__sohaibcompartmentlinear(ev.top,series,aif,roi,in) THEN RETURN

	PMI__Message, status, 'Calculating..'

	Dom = {z:Series->z(), t:Series->t(0), m:Series->m()}

    Sev = Stdy->New('SERIES', Domain= Dom,  Name= '1C: Extracellular Volume (ml/100ml)' )
    Stt = Stdy->New('SERIES', Domain= Dom,  Name= '1C: Mean Transit Time (sec)' )
    Sbf = Stdy->New('SERIES', Domain= Dom,  Name= '1C: Blood Flow (ml/min/g)' )

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

    			P = P-P0

    			BF = fltarr(d[0],d[1])
    			VE = fltarr(d[0],d[1])
    			TT = fltarr(d[0],d[1])

				for r=0L,cnt-1 do begin
   					curve = reform(P[r,*])
					FitToftsLinear, time, aif, curve, ve=ecv, Ktrans=Ktrans
					BF[k[nozero[r]]] = 60E*Ktrans/0.55
					VE[k[nozero[r]]] = 100E*ecv
					TT[k[nozero[r]]] = ecv/Ktrans
				endfor

				Sbf -> Write, Stdy->DataPath(), BF, j
				Sev -> Write, Stdy->DataPath(), VE, j
				Stt -> Write, Stdy->DataPath(), TT, j

			endif
		endif
	endfor

	Sbf -> Trim, 8, 1
	Sev -> Trim, 100, 1
	Stt -> Trim, 10, 1

	ColourTable, 1, Red=R, Green=G, Blue=B
	Sbf -> Clr, R, G, B

    PMI__Control, ev.top, /refresh
end


pro PMI__Button__Control__sohaibcompartmentlinear, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 0)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__sohaibcompartmentlinear, parent,value=value,separator=separator

    if n_elements(value) eq 0 then value = 'One-compartment analysis (Pixel)'

    id = widget_button(parent $
    ,   value = value  $
    ,  	event_pro = 'PMI__Button__Event__sohaibcompartmentlinear' $
    ,	pro_set_value = 'PMI__Button__Control__sohaibcompartmentlinear' $
    ,  	separator = separator )

    return, id
end

