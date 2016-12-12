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


function fname, file, slash

	if n_params() eq 1 then slash = '\'

	path = fpath(file,slash)
	parts 	= str_sep(file,path)
	parts 	= str_sep(parts[1],'.')
	n 		= n_elements(parts)
	if n eq 1 then return, parts[0]
	name = parts[0]
	for i=1L,n-2 do name = name + '.' + parts[i]
	if strlen(parts[n-1]) ne 3 then name = name + '.' + parts[n-1]
	return, name
end