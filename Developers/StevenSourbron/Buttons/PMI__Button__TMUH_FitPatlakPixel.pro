function PMI__Button__Input__TMUH_FitPatlakPixel, top, series, aif, roi

    PMI__Info, top, Stdy=Stdy
    DynSeries = Stdy->Names(0,DefDim=3,ind=ind,sel=sel)
	in = {ser:sel, aif:stdy->sel(1), roi:stdy->sel(0)}

	WHILE 1 DO BEGIN

		in = PMI__Form(top, Title='Perfusion analysis setup', [$
		ptr_new({Type:'DROPLIST',Tag:'ser', Label:'Dynamic series', Value:DynSeries, Select:in.ser}), $
		ptr_new({Type:'DROPLIST',Tag:'aif', Label:'Arterial Region', Value:Stdy->names(1), Select:in.aif}), $
		ptr_new({Type:'DROPLIST',Tag:'roi', Label:'Tissue Region', Value:Stdy->names(1), Select:in.roi})])
		IF in.cancel THEN return, 0

    	Series = Stdy->Obj(0,ind[in.ser])

	   		Aif = PMI__RoiCurve(Stdy->DataPath(), Series, Stdy->Obj(1,in.aif), status, cnt=cnt)
    		IF cnt EQ 0 THEN $
    			msg = ['Arterial region is empty on this series',$
    			'Please select another region and/or series'] $
    		ELSE IF n_elements(Aif) NE Series->d(3) THEN $
    			msg = ['Arterial region is not defined on every dynamic',$
    			'Please select another region and/or series'] $
    		ELSE BEGIN
    			Aif = LMU__Enhancement(Aif,1,relative=0)/(1-0.45)
				Roi = Stdy->Obj(1,in.roi)
				return, 1
    		ENDELSE

     	IF 'Cancel' EQ dialog_message(msg,/information,/cancel) THEN return, 0
  	ENDWHILE
end

pro PMI__Button__Event__TMUH_FitPatlakPixel, ev

   PMI__Info, ev.top, Status=Status, Stdy=Stdy
   PMI__Message, status, 'Preparing calculation..'

   if not PMI__Button__Input__TMUH_FitPatlakPixel(ev.top, Series, aif, roi) then return

  	PMI__Message, status, 'Preparing calculation..'

	Dom = {z:Series->z(), t:Series->t(0), m:Series->m()}
  	Sur = Stdy->New('SERIES', Domain=Dom, Name='Intracellular Uptake Rate (ml/100ml/min)')
  	Sev = Stdy->New('SERIES', Domain=Dom, Name='Extracellular Volume (ml/100ml)')

	Sur->Trim, 0E, 1
	Sev->Trim, 0E, 1

	d = Series->d()
	time = Series->t() - Series->t(0)
	VE = fltarr(d[0]*d[1])
	UR = fltarr(d[0]*d[1])

   	for j=0L,d[2]-1 do begin

		PMI__Message, status, 'Calculating ', j/(d[2]-1E)
		P = PMI__PixelCurve(Stdy->DataPath(),Series,Roi,z=Series->z(j),cnt=cnt,ind=k)

		if cnt gt 0 then begin
			P0 = reform(P[*,0])
			P0 = rebin(P0,cnt,d[3])
			P = P-P0
  			for i=0L,cnt-1 do begin

     			PMI__Message, status, 'Fitting pixels to Patlak model', i/(cnt-1.0)

				Pars = [0.1, 4.5/6000] ;[VE, UR]
				tmp = FitSingleInlet('Patlak', time, aif, reform(p[i,*]), Pars, /POSITIVITY, LIMITED_ABOVE=[1,0])

     			VE[k[i]] = 100.0*Pars[0]
     			UR[k[i]] = 6000.0*Pars[1]
  			endfor

			Sur->Write, Stdy->DataPath(), UR, j
			Sev->Write, Stdy->DataPath(), VE, j

			Sur->Trim, max([Sur->Trim(1),max(UR)]), 1
			Sev->Trim, max([Sev->Trim(1),max(VE)]), 1

			VE=VE*0
			UR=UR*0
		endif
	endfor

  	PMI__Control, ev.top, /refresh
end


pro PMI__Button__Control__TMUH_FitPatlakPixel, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 1)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__TMUH_FitPatlakPixel, parent,value=value, separator=separator

	SingleInletPatlak

	if n_elements(value) eq 0 then value = 'Pixel fit to Patlak model'

	id = widget_button(parent $
	,	value 		= value	$
	,	event_pro 	= 'PMI__Button__Event__TMUH_FitPatlakPixel'$
	,	pro_set_value 	= 'PMI__Button__Control__TMUH_FitPatlakPixel' $
	, 	separator 	= separator	)

	return, id

end
