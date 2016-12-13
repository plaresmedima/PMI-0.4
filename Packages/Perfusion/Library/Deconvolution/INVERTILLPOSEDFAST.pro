;
;
;    Copyright (C) 2005 Steven Sourbron
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

FUNCTION INVERTILLPOSEDFASTINV, W, REGPAR=par, REGPROC=reg

	if n_elements(reg) eq 0 then reg = 'TSVD'
	if n_elements(par) eq 0 then par = 0.15

	m = max(W)*par

	case reg of
		'STIK': D = W/(W^2+m^2)
		'TSVD': begin
			D = fltarr(n_elements(W))
			i = where(W gt m,n)
			if n gt 0 then D[i]=1/W[i]
			end
	endcase

	return, D
END


PRO INVERTILLPOSEDFAST, B, A, ID=id, _EXTRA=e

	PMI__Message, id, 'Deconvolving..'

	svdc, A, W,U,V, /double

	r = n_elements(A[0,*])
	c = n_elements(A[*,0])

;r must LE c

	D = INVERTILLPOSEDFASTINV(W[0:r-1],_EXTRA=e)
	X = transpose(U[0:r-1,*])
	for i=0L,r-1 do X[*,i] = D[i]*X[*,i]
	X = V[0:r-1,*] ## X

	B = X ## B
END