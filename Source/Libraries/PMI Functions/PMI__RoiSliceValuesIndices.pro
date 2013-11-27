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


FUNCTION PMI__RoiSliceValuesIndices, Path, S, R, i, j, cnt=cnt, ind=ri

	cnt = 0L

	dS = S->d()
	dR = R->d()

	if not arrcomp(dS[0:1],dR[0:1]) then return, 0B

    ri = R -> Where(Path,i,j,n=cnt)

    if cnt eq 0 then return, 0B

    v = S -> Read(Path,i,j)

	if size(v,/n_dimensions) ne 0 then return, v[ri]

	cnt = 0L
	return, 0B

END