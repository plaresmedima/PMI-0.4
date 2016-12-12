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


FUNCTION PMI__RoiCurveSdev, Path, S, R, id, X=X, cnt=cnt

    d = R->d()

    Y = fltarr(d[3])
    X = R->t()
    i = make_array(d[3],value=-1)

    for k=0L,d[3]-1 do begin

       PMI__Message, id, 'Loading curve of ' + R->name(), k/(d[3]-1E)

       v = PMI__RoiVolumeValues(Path,S,R,X[k],cnt=n)

       if n gt 1 then begin
         Y[k] = stddev(v)
         i[k] = k
       endif

    endfor

    PMI__Message, id

    k = where(i ne -1, cnt)
    if cnt gt 0 then begin
       X = X[k]
       Y = Y[k]
    endif

    return, Y
END