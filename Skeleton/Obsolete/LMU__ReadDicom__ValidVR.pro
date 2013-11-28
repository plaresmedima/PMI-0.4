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


function LMU__ReadDicom__ValidVR, vr

	case vr of

	 	'FL':return, 1B
		'FD':return, 1B
		'SL':return, 1B
		'SS':return, 1B
		'UL':return, 1B
		'US':return, 1B
		'AE':return, 1B
		'AS':return, 1B
		'CS':return, 1B
		'DA':return, 1B
		'DS':return, 1B
		'DT':return, 1B
		'IS':return, 1B
		'LO':return, 1B
		'LT':return, 1B
		'PN':return, 1B
		'SH':return, 1B
		'ST':return, 1B
		'TM':return, 1B
		'UI':return, 1B
		'UT':return, 1B
		'AT':return, 1B
		'OB':return, 1B
		'OF':return, 1B
		'OW':return, 1B
		'UN':return, 1B
		'SQ':return, 1B

		else: return, 0B
	endcase
end