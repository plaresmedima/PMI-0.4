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


FUNCTION PMI__RoiCurve, Path, S, R, id, X=X, cnt=cnt, slice=slice, INDICES=ind

	d = S->d()

	;Load ROI curve in vector Y
	;N is an array with the number of pixels at each time point

	X = S->t()
    Y = fltarr(d[3])
    N = lonarr(d[3])

	for i=0L,d[2]-1 do begin

		PMI__Message, id, 'Loading ROI curve of ' + R->name(), i/(d[2]-1E)

		Yi = Y*0
    	Ni = N*0

    	for j=0L,d[3]-1 do begin
       		v = PMI__RoiSliceValues(Path,S,R,S->z(i),S->t(j),cnt=cnt)
       		Ni[j] = cnt
       		Yi[j] = total(v)
    	endfor

		k = where(Ni gt 0, cnt)

       	if cnt eq 1 then begin ; If the region is defined at 1 time point only, then copy to all other time points
       		Ni[*] = Ni[k[0]]
			Ind = R->Where(Path,z=S->z(i),t=S->t(k[0]))
	    	for j=0L,d[3]-1 do begin
    			v = S->Read(Path,z=S->z(i),t=S->t(j))
    			Yi[j] = total(v[Ind])
    		endfor
       	endif

		if total(Ni) gt 0 then slice=i

		N = N + Ni
		Y = Y + Yi
   	endfor

    ind = where(N gt 0, cnt)
    if cnt gt 0 then begin
       X = X[ind]
       Y = Y[ind]/N[ind]
    endif

    return, Y

END