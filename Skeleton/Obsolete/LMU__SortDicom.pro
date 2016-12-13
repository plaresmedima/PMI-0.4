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


function LMU__SortDicom, files, p, q, p=z, q=t


	if n_params() eq 1 then begin
		p = ['0020'x,'1041'x]	;Slice location
	;	q = ['0008'x,'0032'x]	;Acquisition time
	;	q = ['0020'x,'0100'x]	;Temporal position identifier
		q = ['0020'x,'0013'x]	;Image Number
	endif



	;Get parameters


	z = PMI__Dicom__Read(files,p[0],p[1])
	t = PMI__Dicom__Read(files,q[0],q[1])


	;Sort



	s = sort2(z,t,n=n)

	files 	= files[s]
	z 		= z[s]
	t 		= t[s]



	;Create 2-D matrix of files

;	iz = uniq(z,sort(z))

	z = reduce(z,iz,n=nz)
	nt = max(n,it)
	t = t[iz[it]:iz[it]+nt-1]

	sfiles = strarr(nz,nt)
	k=0L
	for i=0L,nz-1 do begin
		for j=0L,n[i]-1 do sfiles[i,j] = files[k+j]
		k = k+n[i]
		for j=n[i],nt-1 do sfiles[i,j] = ''
	endfor


	;Remove duplicates


	tred = reduce(t,it)
	if not arrcomp(t,tred) then begin
		sfiles = sfiles[*,it]
		t = tred
	endif


	return, sfiles
end



