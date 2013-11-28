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

pro FitModifiedToftsLinearDelay, t, cp, C, vp=vp, ve=ve, Ktrans=Ktrans, Fit=Fit, DELAY_PAR=delay, DELAY_VALUES=s, AKAIKE_ERROR=aic, POSITIVITY=Pos, LIMITED_ABOVE=above

	n = 1 + floor((s[1]-s[0])/s[2])
	Delay = s[0] + s[2]*findgen(n)
	Error = Delay*0

	for i=0L,n-1 do begin
		If Delay[i] gt 0 then cp_del = ShiftAif(cp,t,Delay[i]) else cp_del = cp
		If Delay[i] lt 0 then C_del = ShiftAif(C,t,-Delay[i]) else C_del = C
		FitModifiedToftsLinear, t, cp_del, C_del, Fit=Fit, AKAIKE_ERROR=aic, POSITIVITY=Pos, LIMITED_ABOVE=above
		Error[i] = aic
	endfor

	tmp = min(Error,i)
	Delay = Delay[i]
	If Delay gt 0 then cp_del = ShiftAif(cp,t,Delay) else cp_del = cp
	If Delay lt 0 then C_del = ShiftAif(C,t,-Delay) else C_del = C
	FitModifiedToftsLinear, t, cp_del, C_del, vp=vp, ve=ve, Ktrans=Ktrans, Fit=Fit, AKAIKE_ERROR=aic, POSITIVITY=Pos, LIMITED_ABOVE=above
	If Delay lt 0 then Fit = interpol(Fit,t,t-Delay)

	aic = aic + 2D
end

pro FitModifiedToftsLinear, t, cp, C, vp=vp, ve=ve, Ktrans=Ktrans, Fit=Fit, DELAY_PAR=delay, DELAY_VALUES=s, AKAIKE_ERROR=aic, POSITIVITY=Pos, LIMITED_ABOVE=above

	IF n_elements(s) NE 0 THEN BEGIN	;Fit with delay
		FitModifiedToftsLinearDelay, t, cp, C, vp=vp, ve=ve, Ktrans=Ktrans, Fit=Fit, DELAY_PAR=delay, DELAY_VALUES=s, AKAIKE_ERROR=aic, POSITIVITY=Pos,  LIMITED_ABOVE=above
		return
	END

	n = n_elements(t)

	A = fltarr(3,n)
	A[0,*] = cp
	A[1,*] = IntVector(t,cp)
	A[2,*] = -IntVector(t,C)

	svdc, A, W,U,V
	for i=0,2 do U[i,*] = U[i,*]/W[i]
	X = V ## transpose(U) ## C

	vd = X[1]/X[2]
	vpf = X[0]*X[2]/X[1]
	Ktrans = X[1]*(1-vpf)

	if keyword_set(Pos) then begin
		if vd lt 0 then vd=0.0
		if vpf lt 0 then vpf=0.0
		if vpf gt 1 then vpf=1.0
		if Ktrans lt 0 then Ktrans=0.0
	endif
	if keyword_set(above) then begin
		if vd gt 1 then vd=1.0
	endif

	vp = vpf*vd
	ve = (1-vpf)*vd

	if arg_present(Fit) then begin
		Fit = vp*cp + Ktrans*ExpConvolution(Ktrans/ve,[t,cp])
		Aic = n*alog(total((C-Fit)^2)/n) + 2D*(1+3)
	endif

end