pro PMI__Button__Event__FitSingleInletUptakeFiltration, ev

  if not TumorPixelAnalysis__input( $
     ev.top $
     ,	Stdy = Stdy $
     ,	status = status $
     ,	time = time $
     ,	pcurve = curve $
     ,	aif = aif $
     ,	Roi = Roi $
     ,	Series = series $
     ,	ev =ev $
     ) then return

	PMI__Message, status, 'Preparing calculation..'

	d = Series->d()
  	n = mult(d[0:2])
  	PF 	= fltarr(n)
  	PT 	= fltarr(n)
  	PV 	= fltarr(n)
  	EF 	= fltarr(n)
  	Ex 	= fltarr(n)

    i=0L
    ind = Roi -> Where(Stdy->DataPath(),t=Roi->t(i),n=n)
    while n eq 0 do begin
    	i=i+1
    	ind = Roi -> Where(Stdy->DataPath(),t=Roi->t(i),n=n)
    endwhile

  	for i=0L,n-1 do begin

     PMI__Message, status, 'Fitting pixels to Uptake model', i/(n-1.0)

   	 P = [0.1, 120.0/6000, 12/120.] ;[VP, FP, FE/FP]
	 Fit = FitSingleInlet('2CUptakeFiltration', time, aif, reform(curve[i,*]), P)

     j = ind[i]

     PF[j] 	= 6000E*P[1]
     PT[j] 	= 1E*P[0]/P[1]
     PV[j] 	= 100E*P[0]
     EF[j] 	= 6000E*P[1]*P[2]
     Ex[j] 	= 100E*P[2]

  	endfor

  	PMI__Message, status, 'Saving Results'

  	Dom = {z:Series->z(), t:Series->t(0), m:Series->m()}

  	S = Stdy->New('SERIES', Data=PF, Domain=Dom, Name='Plasma Flow (ml/100ml/min)')
  	S = Stdy->New('SERIES', Data=PV, Domain=Dom, Name='Plasma Volume (ml/100ml)')
  	S = Stdy->New('SERIES', Data=PT, Domain=Dom, Name='Plasma MTT (ml/100ml)')
  	S = Stdy->New('SERIES', Data=EF, Domain=Dom, Name='Permeability-surface area product (ml/100ml/min)')
  	S = Stdy->New('SERIES', Data=Ex, Domain=Dom, Name='Extraction Fraction (%)')

  	PMI__Control, ev.top, /refresh
end

pro PMI__Button__Control__FitSingleInletUptakeFiltration, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 1)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__FitSingleInletUptakeFiltration, parent,value=value, separator=separator

  if n_elements(value) eq 0 then value = 'Fit single-inlet uptake filtration model (Pixel)'

  id = widget_button(parent $
  ,		value= value $
  ,		event_pro= 'PMI__Button__Event__FitSingleInletUptakeFiltration'$
	,	pro_set_value 	= 'PMI__Button__Control__FitSingleInletUptakeFiltration' $
  ,		separator= separator)

  return, id

end
