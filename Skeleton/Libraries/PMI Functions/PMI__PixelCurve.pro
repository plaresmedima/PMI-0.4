;Loads the pixel curves of SERIES 'S' and ROI 'R' of the slice with slice position 'z'

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


FUNCTION PMI__SlicePixelCurve, Path, S, R, z, id, X=X, cnt=n, ind=ind

    d = S->d()
    X = S->t()

    k0=0L & n=0L
    while n eq 0 do begin
    	if k0 eq d[3] then return, 0B
       	PMI__Message, id, 'Loading pixel curves of ' + R->name(), k0/(d[3]-1E)
       	v = PMI__RoiSliceValues(Path,S,R,z,S->t(k0),cnt=n,ind=ind)
       	k0 = k0+1
    endwhile

    Nk = bytarr(d[3])
    C = fltarr(n,d[3])
    Nk[k0-1] = 1B
    C[*,k0-1] = v
    for k=k0,d[3]-1 do begin
       PMI__Message, id, 'Loading pixel curves of ' + R->name(), k/(d[3]-1E)
       v = PMI__RoiSliceValues(Path,S,R,z,S->t(k),cnt=cnt)
       Nk[k] = cnt eq n
       if cnt eq n then C[*,k] = v $
       else if cnt gt 0 then begin
       		PMI__Message, id, 'Region does not have constant number of pixels!'
       		n=0L
            return, 0B
       endif
    endfor



    PMI__Message, id

    i = where(Nk eq 1B, cnt)
    if cnt gt 1 then begin
       X = X[i]
       return, C[*,i]
    endif


    PMI__Message, id, 'Loading pixel curves of ' + R->name()
    for k=0L,d[3]-1 do begin
    	v = S -> Read(Path,pos=[z,S->t(k)])
        C[*,k] = v[ind]
    endfor

    PMI__Message, id

    return, C
END




FUNCTION PMI__PixelCurve, Path, S, R, id, z=z, X=X, cnt=n, ind=ind

    if n_elements(z) ne 0 then return, PMI__SlicePixelCurve(Path,S,R,z,id,X=X,cnt=n, ind=ind)

    d = S->d()
    X = S->t()

    k0=0L & n=0L
    while n eq 0L do begin
    	if k0 eq d[3] then return, 0B
    	PMI__Message, id, 'Loading pixel curves of ' + R->name(), k0/(d[3]-1E)
       	v = PMI__RoiVolumeValues(Path,S,R,S->t(k0),cnt=n,ind=ind)
       	k0 = k0+1
    endwhile

    Nk = bytarr(d[3])
    C = fltarr(n,d[3])
    Nk[k0-1] = 1B
    C[*,k0-1] = v
    for k=k0,d[3]-1 do begin
       PMI__Message, id, 'Loading pixel curves of ' + R->name(), k/(d[3]-1E)
       v = PMI__RoiVolumeValues(Path,S,R,S->t(k),cnt=cnt)
       Nk[k] = cnt eq n
       if cnt eq n then C[*,k] = v $
       else if cnt gt 0 then begin
       		PMI__Message, id, 'Region does not have constant number of pixels!'
       		n=0L
       		return, 0B
       endif
    endfor



    PMI__Message, id

    i = where(Nk eq 1B, cnt)
    if cnt gt 1 then begin
       X = X[i]
       return, C[*,i]
    endif



    PMI__Message, id, 'Loading pixel curves of ' + R->name()

    for k=0L,d[3]-1 do begin
    	v = S -> Read(Path,t=S->t(k))
        C[*,k] = v[ind]
    endfor

    PMI__Message, id

    return, C
END