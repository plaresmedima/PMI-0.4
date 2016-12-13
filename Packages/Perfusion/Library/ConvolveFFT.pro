;Calculates a convolution in first order using the FFT

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

FUNCTION ConvolveFFT, dt, f, g

	N = n_elements(f)

	Ff = 2*N * FFT([f,fltarr(N)], -1)
	Fg = 2*N * FFT([g,fltarr(N)], -1)

	c = FFT(Ff*Fg, +1)/(2*N)

	return, dt * REAL_PART(c[0:n-1])
END