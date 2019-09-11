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


pro PMI__Button__Event__Jeong_SemiQuantitativePerfusion, ev

	PMI__Info, ev.top, Status=Status, Stdy=Stdy
	PMI__Message, Status, 'Preparing calculation'

	;GET USER INPUT

    Series = Stdy->Names(0,ns,DefDim=3,ind=ind,sel=sel)

	v = PMI__Form(ev.top, Title='Semi-quantitative Perfusion', $
	[ ptr_new({Type:'DROPLIST',Tag:'series',Label: 'Dynamic series', Value:Series, Select:sel}) $
	, ptr_new({Type:'VALUE'	,Tag:'nt',Label: 'Length of baseline (sec)', Value:10.0}) ] )
	if v.cancel then return

	PMI__Message, Status, 'Preparing calculation'

	Series = Stdy->Obj(0,ind[v.series])

	;CREATE OUTPUT

	MAX = Stdy->New('SERIES',Default=Series,Name=Series->Name()+'[Maximum concentration (mM)]') & MAX->t, Series->t(0)
	AUC = Stdy->New('SERIES',Default=Series,Name=Series->Name()+'[Average concentration (mM)]')	& AUC->t, Series->t(0)
	MTT = Stdy->New('SERIES',Default=Series,Name=Series->Name()+'[Area:Maximum]')				& MTT->t, Series->t(0)

	MAX->Trim, [0E, 4E]
	AUC->Trim, [0E, 2E]
	MTT->Trim, [0E, 1E]

	;START OF CALCULATION

	time = Series->t()
	time = float(time-time[0])
	d = Series->d()
	nb = ceil(v.nt/time[1])

	tmax = 100.0 ;sec
    tmp = Min(Abs(time - tmax), max_t) ;integrate over first 100s

	MaxIm = fltarr(d[0]*d[1])
	AUCim = fltarr(d[0]*d[1])
	MTTim = fltarr(d[0]*d[1])

	;LOOP OVER THE SLICES

	for i=0L,d[2]-1 do begin

		;calculate baseline P0
		P0 = Series -> Read(Stdy->DataPath(),i,0)
		for j=1L,nb-1 do P0 = P0 + Series -> Read(Stdy->DataPath(),i,j)
		P0 = P0/nb
		nozero = where(P0 NE 0, cnt_nozero)

		if cnt_nozero GT 0 then begin
		    for j=0L,max_t do begin

			    PMI__Message, Status, 'Calculating', (i*d[3]+j)/(d[2]*d[3]-1E)
			    P = Series -> Read(Stdy->DataPath(),i,j)
				P = (P[nozero]/P0[nozero]-1) / (3.6 * 0.809) ;assume liver T1=809ms and relaxivity 3.6 Hz/mM

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

			AUCim[nozero] = AUCim[nozero]/tmax
            MTTim[nozero] = remove_inf(AUCim[nozero]/MaxIm[nozero])
        endif

		MAX -> Write, Stdy->DataPath(), MaxIm, i
		AUC -> Write, Stdy->DataPath(), AUCim, i
		MTT -> Write, Stdy->DataPath(), MTTim, i

        if cnt_nozero GT 0 then begin

		    MaxIm[nozero] = 0
		    AUCim[nozero] = 0
		    MTTim[nozero] = 0
		endif

	endfor

	PMI__Control, ev.top, /refresh
end



pro PMI__Button__Control__Jeong_SemiQuantitativePerfusion, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		sensitive = ns gt 0
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__Jeong_SemiQuantitativePerfusion, parent,value=value, separator=separator

	if n_elements(value) eq 0 then value = 'Semi-quantitative parameters (Pixel)'

    id = widget_button(parent $
    , 	value 		= value $
	, 	event_pro	= 'PMI__Button__Event__Jeong_SemiQuantitativePerfusion' $
	,	pro_set_value 	= 'PMI__Button__Control__Jeong_SemiQuantitativePerfusion' $
	, 	separator 	= separator	)

	return, id
end

