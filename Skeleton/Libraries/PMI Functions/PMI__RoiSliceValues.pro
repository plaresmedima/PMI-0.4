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


FUNCTION PMI__RoiSliceValues, Path, S, R, z, t, cnt=cnt, ind=i

	cnt = 0L

	dS = S->d()
	dR = R->d()

	if not arrcomp(dS[0:1],dR[0:1]) then return, 0B

    i = R -> Where(Path,pos=[z,t],n=cnt)

    if cnt eq 0 then return, 0B

    v = S -> Read(Path,pos=[z,t])

	if size(v,/n_dimensions) ne 0 then return, v[i]

	cnt = 0L
	return, 0B

END