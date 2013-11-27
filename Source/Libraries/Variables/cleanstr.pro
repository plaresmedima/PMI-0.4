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


function cleanstr, str

		name2 = ''
		for i=0L,strlen(str)-1 do begin
			char = strmid(str,i,1)
			case char of
				'/': char = '_'
				'\': char = '_'
				':': char = '_'
				'*': char = '_'
				'?': char = '_'
				'"': char = '_'
				'<': char = '_'
				'>': char = '_'
				'|': char = '_'
				else:
			endcase
			name2 = name2 + char
		endfor
		return, name2

end