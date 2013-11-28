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


function change_base, x, b

	xl = long(x)
	bl = long(b)

	y = xl/bl
	a = xl-bl*y
	xl = y

	while xl gt 0 do begin
		y = xl/bl
		a = [xl-bl*y,a]
		xl = y
	endwhile


	if bl eq 16 then begin
		i10 = where(a eq 10,cnt10)
		i11 = where(a eq 11,cnt11)
		i12 = where(a eq 12,cnt12)
		i13 = where(a eq 13,cnt13)
		i14 = where(a eq 14,cnt14)
		i15 = where(a eq 15,cnt15)
		a = strcompress(a,/remove_all)
		if cnt10 gt 0 then a[i10]='A'
		if cnt11 gt 0 then a[i11]='B'
		if cnt12 gt 0 then a[i12]='C'
		if cnt13 gt 0 then a[i13]='D'
		if cnt14 gt 0 then a[i14]='E'
		if cnt15 gt 0 then a[i15]='F'
		a = strjoin(a)
	endif

	return, a
end