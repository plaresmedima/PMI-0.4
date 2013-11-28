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


pro PMI__Button__Event__FitSingleInletUptakeExchange, ev


  if not TumorPixelAnalysis__input( $
     ev.top $
     ,	Stdy = Stdy $
     ,	status = status $
     ,	time = time $
     ,	pcurve = p $
     ,	aif = aif $
     ,	units = units $
     ,	Roi = Roi $
     ,	Series = series $
     ,	ev =ev $
     ) then return

	PMI__Message, status, 'Preparing calculation..'

	d = Roi->d()
  	n = mult(d[0:2])
  	PF 	= fltarr(n)
  	PT 	= fltarr(n)
  	PV 	= fltarr(n)
  	EF 	= fltarr(n)

    i=0L
    ind = Roi -> Where(Stdy->DataPath(),t=Roi->t(i),n=n)
    while n eq 0 do begin
    	i=i+1
    	ind = Roi -> Where(Stdy->DataPath(),t=Roi->t(i),n=n)
    endwhile

  	for i=0L,n-1 do begin

     PMI__Message, status, 'Fitting pixels to uptake model', i/(n-1.0)

   	 Pars = [0.1, 0.02, 0.1] ;[VP, FP, FE/(FP+FE)]
	 Fit = FitSingleInlet('2CUptakeExchange', time, aif, reform(p[i,*]), Pars)

     j = ind[i]

     PF[j] 	= 6000E*Pars[1]
     PT[j] 	= 1E*Pars[0]*(1-Pars[2])/Pars[1]
     PV[j] 	= 100E*Pars[0]
     EF[j] 	= 6000E*Pars[1]*Pars[2]/(1-Pars[2])

  	endfor

  	PMI__Message, status, 'Saving Results'

  	Dom = {z:Series->z(), t:Series->t(0), m:Series->m()}

  	S = Stdy->New('SERIES', Data=PF, Domain=Dom, Name='Plasma Flow (ml/100ml/min)')
  	S = Stdy->New('SERIES', Data=PV, Domain=Dom, Name='Plasma Volume (ml/100ml)')
  	S = Stdy->New('SERIES', Data=PT, Domain=Dom, Name='Plasma MTT (ml/100ml)')
  	S = Stdy->New('SERIES', Data=EF, Domain=Dom, Name='Permeability-surface area product (ml/100ml/min)')

  	PMI__Control, ev.top, /refresh
end

pro PMI__Button__Control__FitSingleInletUptakeExchange, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 1)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__FitSingleInletUptakeExchange, parent,value=value, separator=separator

  if n_elements(value) eq 0 then value = 'Fit single-inlet uptake exchange model (Pixel)'

  id = widget_button(parent $
  ,		value= value $
  ,		event_pro= 'PMI__Button__Event__FitSingleInletUptakeExchange'$
	,	pro_set_value 	= 'PMI__Button__Control__FitSingleInletUptakeExchange' $
  ,		separator= separator)

  return, id

end
