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


FUNCTION COSTFUNCTIONOPTIMIZE, M,C

	n = n_elements(C)
	k = n-2

	while (C[k] ge C[k-1]) or (C[k] ge C[k+1]) do begin
		k = k-1
		if k eq 0 then return, M[n-1]
	endwhile

	return, M[k]
END
PRO SMOOTHCOSTFUNCTION, C, wm=wm, ws=ws, reg=R

	if wm gt 1 then for p=0L,n_elements(C[*,0])-1 do C[p,*]=smooth(C[p,*],wm,/edge_truncate)
	if ws eq 1 then return

	X = float(R)
	Y = where(R)

	for k=0L,n_elements(C[0,*])-1 do begin

		X[Y] = C[*,k]
		for j=0L,n_elements(X[0,0,*])-1 do X[*,*,j]=smooth(X[*,*,j],ws,/edge_truncate)
		C[*,k] = X[Y]
		X = X*0
	endfor

END
FUNCTION REGPARVALUES, Wmax, m0=m0,m1=m1,nm=nm

	if n_elements(m0) eq 0 then m0=0.001
	if n_elements(m1) eq 0 then m1=10.0
	if n_elements(nm) eq 0 then nm=100

	return, Wmax * m0 * (m1/m0)^(findgen(nm)/(nm-1))
END




;LCC





FUNCTION LCC, l,r,e,d

	k = 2*(e*r/d)
	k = k*(l^2.0*d*r + 2.0*l*e*r + l^4.0*e*d)
	k = k/((l^4.0*e^2+r^2.0)^1.5)

	return, k
END
FUNCTION LCCOPTIMIZE, M, L, C

	n = n_elements(M)
	k = n-2
	X = LCC(M[k-1:k+1], L[*,k-1:k+1,0]##C, L[*,k-1:k+1,1]##C, L[*,k-1:k+1,2]##C)

	while (X[1] ge X[0]) or (X[1] ge X[2]) do begin
		k = k-1
		if k eq 0 then return, M[n-1]
		X[1:2] = X[0:1]
		X[0] = LCC(M[k-1], L[*,k-1,0]##C, L[*,k-1,1]##C, L[*,k-1,2]##C)
	endwhile

	return, M[k]
END
FUNCTION LCCMATRIX, M,Wsq

	n = n_elements(M)
	L = fltarr(n_elements(Wsq),n,3)

	Msq = M^2
	for k=0L,n-1 do begin
		Nom = Wsq + Msq[k]
		L[*,k,0] = (Msq[k]/Nom)^2
		L[*,k,1] = Wsq / Nom^2
		L[*,k,2] = -4*M[k]*Wsq / Nom^3
	endfor

	return, L
END
FUNCTION LCCREGPARPLUS,B,W,M,ID=id,wm=wm,ws=ws,reg=r

	n = n_elements(B[*,0])
	C = fltarr(n,n_elements(M))
	O = fltarr(n)
	L = LCCMATRIX(M,W^2)

	for p=0L,n-1 do begin
	;	PMI__Message, id, 'Calculating LCC ', p/(n-1E)
		X = B[p,*]^2
		C[p,*] = LCC(M,L[*,*,0]##X,L[*,*,1]##X,L[*,*,2]##X)
	endfor

;	PMI__Message, id, 'Smoothing LCC '
	SMOOTHCOSTFUNCTION,C,wm=wm,ws=ws,reg=r

	for p=0L,n-1 do begin
;		PMI__Message, id, 'Optimizing LCC ', p/(n-1E)
		O[p] = COSTFUNCTIONOPTIMIZE(M,C[p,*])
		;cw_create_plot, C[p,*], xaxis=M/max(W)
	endfor

	return, O
END
FUNCTION LCCREGPAR,B,W,M,ID=id

	n = n_elements(B[*,0])
	O = fltarr(n)
	L = LCCMATRIX(M,W^2)

	for p=0L,n-1 do begin
;		PMI__Message, id, 'Calculating LCC ', p/(n-1E)
		O[p] = LCCOPTIMIZE(M,L,B[p,*]^2)
		;print, O[p]/max(W)
	endfor

	return, O
END






;GCV





FUNCTION GCVOPTIMIZE, M, G, C

	n = n_elements(M)
	k = n-2
	X = G[*,k-1:k+1] ## C

	while (X[1] ge X[0]) or (X[1] ge X[2]) do begin
		k = k-1
		if k eq 0 then return, M[n-1]
		X[1:2] = X[0:1]
		X[0] = G[*,k-1] ## C
	endwhile

	return, M[k]
END
FUNCTION GCVMATRIX, Msq,Wsq

	n = n_elements(Msq)
	G = fltarr(n_elements(Wsq),n)

	for k=0L,n-1 do begin
		G1 = Wsq + Msq[k]
		G2 = total(1E/G1)
		G[*,k] = (G1*G2)^(-2E)
	endfor

	return, G
END
FUNCTION GCVREGPARPLUS,B,W,M,ID=id,wm=wm,ws=ws,reg=r

	n = n_elements(B[*,0])
	O = fltarr(n)
	G = GCVMATRIX(M^2,W^2)

;	PMI__Message, id, 'Calculating GCV'
	C = G ## B^2

;	PMI__Message, id, 'Smoothing GCV'
	SMOOTHCOSTFUNCTION,C,wm=wm,ws=ws,reg=r

	for p=0L,n-1 do begin
;		PMI__Message, id, 'Optimizing GCV ', p/(n-1E)
		O[p] = COSTFUNCTIONOPTIMIZE(M,C[p,*])
	endfor

	return, O
END
FUNCTION GCVREGPAR,B,W,M,ID=id

	n = n_elements(B[*,0])
	O = fltarr(n)
	G = GCVMATRIX(M^2,W^2)

	for p=0L,n-1 do begin
;		PMI__Message, id, 'Calculating GCV ', p/(n-1E)
		O[p] = GCVOPTIMIZE(M,G,B[p,*]^2)
	endfor

	return, O
END





FUNCTION REGPAR, B, W, ID=id,pc=pc,m0=m0,m1=m1,nm=nm,wm=wm,ws=ws,reg=r

	if n_elements(pc) eq 0 then pc = 'LCC'
	if n_elements(wm) eq 0 then wm = 1
	if n_elements(ws) eq 0 then ws = 1

	PLUS = (ws gt 1) or (wm gt 1)

	M = REGPARVALUES(max(W),m0=m0,m1=m1,nm=nm)

	CASE pc OF
	 'GCV'	:if PLUS then O=GCVREGPARPLUS(B,W,M,ID=id,wm=wm,ws=ws,reg=r) else O=GCVREGPAR(B,W,M,ID=id)
	 'LCC'	:if PLUS then O=LCCREGPARPLUS(B,W,M,ID=id,wm=wm,ws=ws,reg=r) else O=LCCREGPAR(B,W,M,ID=id)
	ENDCASE

	return, O
END



;Algorithm to deconvolve pixel data, as defined in PMB paper




PRO INVERTILLPOSED, B, A, ID=id, _EXTRA=e

;	PMI__Message, id, 'Calculating SVD..'

	svdc, A, W,U,V, /double

;	PMI__Message, id, 'Calculating UT ## B..'

	B = transpose(U) ## B

;	PMI__Message, id, 'Calculating regularization parameters..'

	O = REGPAR(B,W, _EXTRA=e, ID=id)

;	PMI__Message, id, 'Inverting..'

	np = n_elements(O) & Wsq=W^2
	for p=0L,np-1 do B[p,*]=B[p,*]*W/(Wsq+O[p]^2)

;	PMI__Message, id, 'Calculating V ## B..'

	B = V ## B

END