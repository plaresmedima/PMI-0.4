;Returns any number of percentiles for a data vector X
;p is an array listing the desired percentiles (in perc)

;
;    Copyright (C) 2013 Steven Sourbron
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


function percentiles, X, P, InterQuartile=IQ

	if arg_present(IQ) then begin

		Perc = percentiles(X,[25,75])
		IQ = Perc[1]-Perc[0]
	endif

	X = X[sort(X)]
	n = n_elements(X)


	return, X[floor((p/100D)*(n-1D))]
end