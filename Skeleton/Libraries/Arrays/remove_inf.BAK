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


function remove_inf, data

	result = float(data)
	inf = finite(result, /infinity)
	nan = finite(result, /nan)
	fin = finite(result)

	;if there are no finite, then set everything to zero

	if total(inf + nan) eq n_elements(result) then begin
		result[*] = 0.0
		return, result
	endif

	;set -Inf to the minimal value and +Inf to the maximal value

	if total(inf) ne 0 then begin

		maximum = max(result[where(fin eq 1)], min = minimum)
		pos_inf = where((inf eq 1) and (result gt 0), cnt)
		if cnt ne 0 then result[pos_inf] = maximum
		neg_inf = where((inf eq 1) and (result lt 0), cnt)
		if cnt ne 0 then result[neg_inf] = minimum
	endif

	;Set NaN to zero

	if total(nan) ne 0 then result[where(nan eq 1)] = 0.0

	return, result
end
