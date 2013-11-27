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


function PMI__Dicom__Sort, f, z, t

;;;;Sort by z-coordinate

	s = sort(z)
	f = f[s]
	z = z[s]
	t = t[s]
	iz = uniq(z)
	z = z[iz]
	nz = n_elements(z)

;;;;Sort by t-coordinate
;;;;The t-coordinate of the series is taken as the t-coordinate of the slice with most t-points

	k0=0 & k1=iz[0]
	s = sort(t[k0:k1])		;sort times of slice 0
	f[k0:k1] = f[k0 + s]	;sort files of slice 0
	nmax = 1+k1-k0			;nr of time points in slice 0
	tmax = t[k0 + s]		;time coordinates of slice 0
	for i=1L,nz-1 do begin
		k0=iz[i-1]+1 & k1=iz[i]
		s = sort(t[k0:k1])			;sort times of slice i
		f[k0:k1] = f[k0 + s]		;sort files of slice i
		ni = 1+k1-k0				;nr of time points in slice i
		if ni gt nmax then begin 	;if there are more time points in slice i than in previous slices, take t from i
			nmax = ni
			tmax = t[k0 + s]
		endif
	endfor
	t=tmax
	nt=n_elements(t)

;;;;Arrange files in a 2D-matrix
;;;;Missing times will be left blank (zero-filled image)

   	fmat = strarr(nz,nt)
   	k0=0 & k1=iz[0]
   	fmat[0,0:k1-k0] = f[k0:k1]
	for i=1L,nz-1 do begin
		k0=iz[i-1]+1 & k1=iz[i]
		fmat[i,0:k1-k0] = f[k0:k1]
	endfor
	return, fmat



;OLD VERSION - REMOVED DUPLICATE COORDINATES
;;;;;Sort
;
;	s = sort2(z,t,n=n)
;	files 	= files[s]
;	z 		= z[s]
;	t 		= t[s]
;
;;;;;;Create 2-D matrix of files
;
;;	iz = uniq(z,sort(z))
;	z = reduce(z,iz,n=nz)
;	nt = max(n,it)
;	t = t[iz[it]:iz[it]+nt-1]
;
;	sfiles = strarr(nz,nt)
;	k=0L
;	for i=0L,nz-1 do begin
;		for j=0L,n[i]-1 do sfiles[i,j] = files[k+j]
;		k = k+n[i]
;		for j=n[i],nt-1 do sfiles[i,j] = ''
;	endfor
;
;;;;;;Remove duplicates
;
;	tred = reduce(t,it)
;	if not arrcomp(t,tred) then begin
;		sfiles = sfiles[*,it]
;		t = tred
;	endif
;
;	return, sfiles
end