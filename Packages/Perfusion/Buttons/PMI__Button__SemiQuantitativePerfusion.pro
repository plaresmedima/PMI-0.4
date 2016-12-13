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


pro PMI__Button__Event__SemiQuantitativePerfusion, ev

	PMI__Info, ev.top, Status=Status, Stdy=Stdy
	PMI__Message, Status, 'Preparing calculation'

	;GET USER INPUT

    Series = Stdy->Names(0,ns,DefDim=3,ind=ind,sel=sel)

	v = PMI__Form(ev.top, Title='Semi-quantitative Perfusion', $
	[ ptr_new({Type:'DROPLIST',Tag:'series',Label: 'Dynamic series', Value:Series, Select:sel}) $
	, ptr_new({Type:'DROPLIST',Tag:'units',Label: 'Signal model', Value:['Linear (a.u.)','Linear (%)','DSC-MRI'], Select:0}) $
	, ptr_new({Type:'VALUE'	,Tag:'nb',Label: 'Length of baseline (# of dynamics)', Value:1L}) ] )
	if v.cancel then return

	PMI__Message, Status, 'Preparing calculation'

	Series = Stdy->Obj(0,ind[v.series])

	;CREATE OUTPUT

	U = ['a.u.','%','dimensionless'] & U = U[v.units]
	MAX = Stdy->New('SERIES',Default=Series,Name=Series->Name()+'[Maximum ('+U+')]') 					& MAX->t, Series->t(0)
	AUC = Stdy->New('SERIES',Default=Series,Name=Series->Name()+'[Area under the Curve ('+U+'*sec)]')	& AUC->t, Series->t(0)
	MTT = Stdy->New('SERIES',Default=Series,Name=Series->Name()+'[Area:Maximum (sec)]')					& MTT->t, Series->t(0)

	MAX->Trim, 0E, 1
	AUC->Trim, 0E, 1
	MTT->Trim, 0E, 1

	;START OF CALCULATION

	time = Series->t()
	time = float(time-time[0])
	d = Series->d()

	MaxIm = fltarr(d[0]*d[1])
	AUCim = fltarr(d[0]*d[1])
	MTTim = fltarr(d[0]*d[1])

	;LOOP OVER THE SLICES

	for i=0L,d[2]-1 do begin

		;calculate baseline P0
		P0 = Series -> Read(Stdy->DataPath(),i,0)
		for j=1L,v.nb-1 do P0 = P0 + Series -> Read(Stdy->DataPath(),i,j)
		P0 = P0/v.nb
		nozero = where(P0 NE 0, cnt_nozero)

		if cnt_nozero GT 0 then begin
		    for j=0L,d[3]-1 do begin

			    PMI__Message, Status, 'Calculating', (i*d[3]+j)/(d[2]*d[3]-1E)
			    P = Series -> Read(Stdy->DataPath(),i,j)

			    case v.units of
				0:P = P[nozero]-P0[nozero]
				1:P = 100*(P[nozero]/P0[nozero]-1)
				2:P = -remove_inf(alog(P[nozero]/P0[nozero]))
			    endcase

			    if j eq 0 then begin
				    MaxIm[nozero] = P
			    endif else begin
				    ind = where(P gt MaxIm[nozero], cnt)
				    if cnt gt 0 then begin
					    MaxIm[nozero[ind]] = P[ind]
				    endif
				    AUCim[nozero] = AUCim[nozero] + (Time[j]-Time[j-1])*P
			    endelse
			endfor

            MTTim[nozero] = remove_inf(AUCim[nozero]/MaxIm[nozero])
        endif

		MAX -> Write, Stdy->DataPath(), MaxIm, i
		AUC -> Write, Stdy->DataPath(), AUCim, i
		MTT -> Write, Stdy->DataPath(), MTTim, i

		MAX->Trim, max([MAX->Trim(1),max(MaxIm)]), 1
		AUC->Trim, max([AUC->Trim(1),max(AUCim)]), 1
		MTT->Trim, max([MTT->Trim(1),max(MTTim)]), 1

        if cnt_nozero GT 0 then begin

		    MaxIm[nozero] = 0
		    AUCim[nozero] = 0
		    MTTim[nozero] = 0
		endif

	endfor

	PMI__Control, ev.top, /refresh
end



pro PMI__Button__Control__SemiQuantitativePerfusion, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		sensitive = ns gt 0
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__SemiQuantitativePerfusion, parent,value=value, separator=separator

	if n_elements(value) eq 0 then value = 'Semi-quantitative parameters (Pixel)'

    id = widget_button(parent $
    , 	value 		= value $
	, 	event_pro	= 'PMI__Button__Event__SemiQuantitativePerfusion' $
	,	pro_set_value 	= 'PMI__Button__Control__SemiQuantitativePerfusion' $
	, 	separator 	= separator	)

	return, id
end

