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


function RotateBinary, bin, p0, p1

	c0 = CenterOfMass(bin)

	p0c = p0-c0
	p1c = p1-c0

	z = p0c[0]*p1c[1]-p1c[0]*p0c[1]
	sign = - z/abs(z)

	A = norm(p0c)
	B = norm(p1c)
	C = norm(p0-p1)

	angle = ACOS((A^2+B^2-C^2)/(2*A*B))

	b = ROT(bin,sign*angle*180.0/!PI,1.0,c0[0],c0[1],/pivot,missing=0B)

	i = where(b ne 0, cnt)
	if cnt gt 0 then b[i] = 1B

	return, b
end