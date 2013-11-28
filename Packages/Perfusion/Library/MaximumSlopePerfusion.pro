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

function MaximumSlopePerfusion, time, curve, AIF, flow=flow

	cMaxDer = max(deriv(time,curve),i)
	Fit = curve[i] + cMaxDer * (time - time[i])
	i = where(Fit gt max(curve), cnt)
	if cnt gt 0 then Fit[i] = max(curve)
	i = where(Fit lt min(curve), cnt)
	if cnt gt 0 then Fit[i] = min(curve)
	flow = cMaxDer/max(aif)

	return, fit
end