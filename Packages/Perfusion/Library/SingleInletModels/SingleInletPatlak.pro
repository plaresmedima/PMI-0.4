;C(t) = VP Ca(t) + FE * Ca(t)
;P = [VP, FE]
;				Pars[0] is the volume fraction of the blood compartment (dimensionless)
;				Pars[1] is the flow or transfer rate from intra- to extravascular space (in 1/s)


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
Pro SingleInletPatlak, X, P, C, C_DER

	if n_params() eq 0 then return

	ni=X[0] & n=n_elements(X[ni+1:*])/2
	ti=X[1:ni] & time=X[ni+1:ni+n] & input=X[ni+n+1:*]

	Integral = IntVector(time,input)

	C = P[0]*input[ti] + P[1]*Integral[ti]

	IF n_params() LT 4 THEN return

	;Derivatives wrt model parameters

	C_DER0 = input[ti]
	C_DER1 = Integral[ti]

   	C_DER = [[C_DER0],[C_DER1]]
end