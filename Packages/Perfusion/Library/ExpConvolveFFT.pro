

;Calculates a convolution with an exponential in first order using the FFT

;NOTE Laplace Transform of exp(-tK): 1/(s+K)

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


FUNCTION ExpConvolveFFT, K, X

	N = n_elements(X)/2
	g = X[N:*]
	dt = X[1]

	Ntot = N + floor(6E*(1/K)/dt)

	if Ntot/2E eq Ntot/2 then shft=Ntot/2-1 else shft=(Ntot-1)/2

	f = (findgen(Ntot) - shft)/(Ntot*DT)
	s = complex(fltarr(Ntot),2*!PI*f)
	Ff = 1/(s + K)/DT
	Ff = shift(Ff, - shft)

	Fg = Ntot * FFT([g,fltarr(Ntot-N)] , -1)

	c = FFT(Ff*Fg, +1)/Ntot

	return, dt * REAL_PART(c[0:N-1])
END