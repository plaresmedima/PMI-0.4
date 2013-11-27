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


function PMI__nbytes, type

	case type of
		1	:return, 1 	;byte
		2	:return, 2 	;integer
		3	:return, 4 	;longword
		4	:return, 4 	;float
		5	:return, 8 	;double
		7	:return, 1 	;string
		12	:return, 2 	;unsigned int
		13	:return, 4 	;unsigned long
		14	:return, 8 	;64-bit int
		15	:return, 8 	;unsigned 64-bit int
	endcase

end