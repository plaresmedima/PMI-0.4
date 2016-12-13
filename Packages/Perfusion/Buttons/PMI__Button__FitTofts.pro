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


pro PMI__Button__Event__FitTofts, ev

  if not TumorPixelAnalysis__input( $
     ev.top $
     ,	Stdy 	= Stdy $
     ,	status 	= status $
     ,	time 	= time $
     ,	pcurve 	= p $
     ,	aif 	= aif $
     ,	Roi 	= roi $
     ,	Series 	= series $
     ,	ev=ev $
     ) 	then return

  	PMI__Message, status, 'Preparing calculation..'

  	d = Series->d()
  	n = mult(d[0:2])
  	IV = fltarr(n)
  	EF = fltarr(n)

   	i=0L
   	ind = Roi -> Where(Stdy->DataPath(),t=Roi->t(i),n=n)
   	while n eq 0 do begin
    	i=i+1
    	ind = Roi -> Where(Stdy->DataPath(),t=Roi->t(i),n=n)
   	endwhile

  	for i=0L,n-1 do begin

     	PMI__Message, status, 'Fitting pixels to 1-compartment model', i/(n-1.0)

		Pars = [0.3, 120.0/6000] ;[VP+VE, FP]
		p[i,*] = FitSingleInlet('Compartment', time, aif, reform(p[i,*]), Pars)

		j = ind[i]

     	IV[j] = 100.0*Pars[0]
     	EF[j] = 6000.0*Pars[1]
  	endfor


  	PMI__Message, status, 'Saving Results'

	Dom = {z:Series->z(), t:Series->t(0), m:Series->m()}

  	S = Stdy->New('SERIES', Domain=Dom, Data=EF, Name='Plasma Flow (ml/100ml/min)')
  	S = Stdy->New('SERIES', Domain=Dom, Data=IV, Name='Volume of Distribution (ml/100ml)')

  	PMI__Control, ev.top, /refresh
end


pro PMI__Button__Control__FitTofts, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 1)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__FitTofts, parent,value=value, separator=separator

	if n_elements(value) eq 0 then value = 'Pixel fit to Tofts model'

	id = widget_button(parent $
	,	value 		= value	$
	,	event_pro 	= 'PMI__Button__Event__FitTofts'$
	,	pro_set_value 	= 'PMI__Button__Control__FitTofts' $
	, 	separator 	= separator	)

	return, id

end
