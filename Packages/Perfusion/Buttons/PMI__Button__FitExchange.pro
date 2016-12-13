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

pro PMI__Button__Event__FitExchange, ev


	if not TumorPixelAnalysis__input( $
		ev.top $
	,	Stdy = Stdy $
	,	status = status $
	,	time = time $
	,	pcurve = p $
	,	aif = aif $
	,	units = units $
	,	Roi = roi $
	,	Series = series $
	,	ev =ev $
	) 	then return


	PMI__Message, status, 'Preparing calculation..'



	d = Roi->d()
	n = mult(d[0:2])
	PV 	= fltarr(n)
	IV 	= fltarr(n)
	PF 	= fltarr(n)
	EF 	= fltarr(n)


    i=0L
    ind = Roi -> Where(Stdy->DataPath(),t=Roi->t(i),n=n)
    while n eq 0 do begin
    	i=i+1
    	ind = Roi -> Where(Stdy->DataPath(),t=Roi->t(i),n=n)
    endwhile


	for i=0L,n-1 do begin

		PMI__Message, status, 'Fitting pixels to exchange model', i/(n-1.0)

		Pars = [0.3, 0.02, 2.0/3, 0.1] ;[VP+VE, FP, VE/(VP+VE), FE/(FP+FE)]
		Fit = FitSingleInlet('Exchange', time, aif, reform(p[i,*]), Pars)

		j = ind[i]

		PV[j] 	= 100.0*Pars[0]*(1-Pars[2])
		IV[j] 	= 100.0*Pars[0]*Pars[2]
		PF[j] 	= 6000E*Pars[1]
		EF[j] 	= 6000E*Pars[1]*Pars[3]/(1-Pars[3])
	endfor

	PMI__Message, status, 'Saving Results'

	Domain 	= {z:Series->z(), t:Series->t(0), m:Series->m()}

	S = Stdy->New('SERIES',	Domain=Domain, Data=PF,	Name='Plasma Flow (ml/100ml/min)')
	S = Stdy->New('SERIES',	Domain=Domain, Data=PV,	Name='Plasma Volume (ml/100ml)')
	S = Stdy->New('SERIES',	Domain=Domain, Data=EF,	Name='Permeability-surface area product (ml/100ml/min)')
	S = Stdy->New('SERIES',	Domain=Domain, Data=IV,	Name='Interstitial Volume (ml/100ml)')

	PMI__Control, ev.top, /refresh
end



pro PMI__Button__Control__FitExchange, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 1)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__FitExchange, parent,value=value, separator=separator

	if n_elements(value) eq 0 then value = 'Fit 2-compartment exchange model (Pixel)'

	id = widget_button(parent 						$
	,	value 		= value		$
	,	event_pro 	= 'PMI__Button__Event__FitExchange'	$
	,	pro_set_value 	= 'PMI__Button__Control__FitExchange' $
	, 	separator 	= separator						)

	return, id

end
