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

FUNCTION StepConvolution_A1, MTT, t, dt
	IF t GT MTT THEN RETURN, 0E
	IF t+dt LT MTT THEN RETURN, dt*(t+dt/2)
	RETURN, (MTT^2-t^2)/2E
END
FUNCTION StepConvolution_A0, MTT, t, dt
	IF t GT MTT THEN RETURN, 0E
	IF t+dt LT MTT THEN RETURN, dt
	RETURN, MTT-t
END
FUNCTION StepConvolution, R, X
	n = n_elements(X)/2
	Y = dblarr(n)
	IF NOT finite(R) THEN return, Y
	T = X[0:n-1]
	A = X[n:*]
	FOR i=1L,n-1 DO BEGIN
		FOR k=0L,i-1 DO BEGIN
			A0 = StepConvolution_A0(1/R, T[i]-T[k+1], T[k+1]-T[k])
			A1 = StepConvolution_A1(1/R, T[i]-T[k+1], T[k+1]-T[k])
			DA = (A[k+1]-A[k])/(T[k+1]-T[k])
			Y[i] = Y[i] + (A[k] + DA*(T[i]-T[k])) * A0 - DA *A1
		ENDFOR
	ENDFOR
	return, Y
END
