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

function LMU__Enhancement, Y, n, relative=relative

	if n_elements(n) eq 0 then n=1

	Y0 = total(Y[0:n-1])/n

	case relative of
		0:return, Y-Y0	;enhancement (T1)
		1:if Y0 ne 0 then return, (Y-Y0)/Y0		;relative enhancement (T1)
		2:if Y0 ne 0 then return, -alog(Y/Y0)  	;for T2* perfusion
	endcase

	return, Y*0B
end