;Description
;-----------
;
;Calculates the convolution of a given array
;with a mono-exponential function by linear interpolation
;between the values of the array.
;Details can be found in the Appendix of Flouri et al [submitted paper].
;
;
;Syntax
;------
;
;f = EXPCONV(T, time, a)
;
;
;Arguments
;--------
;
;T: the characteristic time of the exponential function exp(-time/T)
;
;time: the time points at which the vector a is defined
;      the first time must be zero
;      must be in the same units as T
;
;a: the array to be interpolated
;   must have the same number of elements as "time"
;
;
;Returns
;-------
;
;f: an array of the same length as "time",
;   containing the result of the convolution
;   at the corresponding times
;
;
;Example
;-------
;
;Convolve an AIF with an normalised exponential with decay time 20s
;
;IDL> tacq = 180.0 & dt = 0.1
;IDL> nt = 1+floor(tacq/dt)
;IDL> t = findgen(nt)*dt
;IDL> a = AIF(t)
;IDL> plot, t, EXPCONV(20., t, a)


;-----------------------------------------------------------------------------
;    Copyright (C) 2015, Steven Sourbron
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
;    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
;-----------------------------------------------------------------------------


FUNCTION EXPCONV, T, time, a

; see appendix of Flouri et al (submitted)
; for details on notations.

 ;   IF T EQ 0 THEN return, a


	n = n_elements(time)
	f = dblarr(n)
	x = (time[1:n-1] - time[0:n-2])/T
	da = (a[1:n-1] - a[0:n-2]) / x

	E = exp(-x)
	E0 = 1-E
	E1 = x-E0

	add = a[0:n-2]*E0 + da*E1

	FOR i=0L,n-2 DO f[i+1] = E[i]*f[i] + add[i]

    RETURN, f

END
