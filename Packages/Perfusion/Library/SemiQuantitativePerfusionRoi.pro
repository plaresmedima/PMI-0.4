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

function SemiQuantitativePerfusionRoi,time,curve,BAT,win=win,Pars=Pars,Units=Units

	n = n_elements(time)
	nb = total(time le BAT)
	S0 = total(curve[0:nb-1])/nb
	Noise = sqrt(total((curve[0:nb-1]-S0)^2)/nb)
	Contrast = abs(max(curve-S0))
	SNR = S0/Noise
	CNR = Contrast/Noise

	case Units of
		'Signal':
		'Signal Enhancement': curve = curve - S0
		'Signal Enhancement (T1)': curve = curve - S0
		'Relative Signal Enhancement': curve = 100.0*(curve-S0)/S0
		'Relative Signal Enhancement (T1)': curve = 100.0*(curve-S0)/S0
		'Relative Signal Enhancement (T2)': curve = -alog(curve/S0)
	endcase

	Fit = curve
	if n_elements(win) ne 0 then begin
		if win gt 2 then Fit = smooth(Fit,win,/edge_truncate)
	endif

	cMax = max(Fit,i)
	tMax = time[i]-BAT
	cMaxDer = max(deriv(time,Fit),i)
	tMaxDer = time[i]-BAT
	cInt = int_tabulated(Time,Fit)
	qMTT = cInt/cMax

	Pars = [cMax,cMaxDer,cInt,qMTT,tMax,tMaxDer,SNR,CNR,S0]

	return, Fit
end