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


function number_of_bytes, v

	n=n_elements(v)
	if n gt 1 then begin
		nb=0L
		for i=0L,n-1 do nb=nb+number_of_bytes(v[i])
		return, nb
	endif

	case size(v,/type) of
		0:return, 0L
		1:return, 1L
		2:return, 2L
		3:return, 4L
		4:return, 4L
		5:return, 8L
		6:return, 8L
		7:return, strlen(v)
		8:begin
			nb=0L
			for i=0L,n_tags(v)-1 do nb=nb+number_of_bytes(v.(i))
			return, nb
		end
		9:return, 16L
		10:return, 0L
		11:return, 0L
		12:return, 2L
		13:return, 4L
		14:return, 8L
		15:return, 8L
	endcase
end