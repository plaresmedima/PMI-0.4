;converts a number 'nr' to a string 'nr_str'
;with a given length 'n' by zero-padding


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


function strnr, nr, n

	nr_str = strcompress(nr,/remove_all)
	nr_zero = n - strlen(nr_str)
	prefix = ''
	for i=0L,nr_zero-1 do prefix = prefix+'0'
	return, prefix + nr_str

end
