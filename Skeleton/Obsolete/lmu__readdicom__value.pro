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


function LMU__ReadDicom__value, unit, vr, length

	if length eq 0 then return, 0B

	;If the length is longer than the nr. of remaining bytes, then return 0
	s = fstat(unit)
	point_lun, -unit, p
	if s.size - p lt length then begin
		point_lun, unit, s.size
		return, 0B
	endif


	;These can be arrays
	single_length = length

	if (vr eq 'SS') $
	or (vr eq 'US') $
	then single_length = 2

;	if (vr eq 'AS') $
;	or (vr eq 'AT') $
	if (vr eq 'FL') $
	or (vr eq 'SL') $
	or (vr eq 'UL') $
	then single_length = 4

;	if (vr eq 'DA') $
	if (vr eq 'FD') $
	then single_length = 8


	if length gt single_length then begin
		n = length/single_length
		value = make_array(n,value=LMU__ReadDicom__value(unit,vr,single_length))
		for i=1,n-1 do value[i]=LMU__ReadDicom__value(unit,vr,single_length)
		return, value
	endif



	case vr of

	 	;these are the numbers

	 	'FL':value = 0E
		'FD':value = 0D
		'SL':value = 0L
		'SS':value = 0S
		'UL':value = 0UL
		'US':value = 0US

		;these are strings

		'AE':value = bytarr(length)
		'AS':value = bytarr(length)
		'CS':value = bytarr(length)
		'DA':value = bytarr(length)
		'DS':value = bytarr(length)
		'DT':value = bytarr(length)
		'IS':value = bytarr(length)
		'LO':value = bytarr(length)
		'LT':value = bytarr(length)
		'PN':value = bytarr(length)
		'SH':value = bytarr(length)
		'ST':value = bytarr(length)
		'TM':value = bytarr(length)
		'UI':value = bytarr(length)
		'UT':value = bytarr(length)

		;the following are arrays of some sort. For VR = OB,OW,SQ or UN the length can be undefined.

		'AT':value = uintarr(length/2)
		'OB':value = bytarr(length)
		'OF':value = fltarr(length/4)
		'OW':value = intarr(length/2)
		'UN':value = bytarr(length)
		'SQ':value = bytarr(length)
	endcase

	readu, unit, value

	case vr of

	 	;these are the numbers

	 	'FL':return, value
		'FD':return, value
		'SL':return, value
		'SS':return, value
		'UL':return, value
		'US':return, value

		;these are strings

		'AE':return, string(value)
		'AS':return, string(value)
		'CS':return, string(value)
		'DA':return, string(value)
		'DS':return, double(string(value))
		'DT':return, string(value)
		'IS':return, long(string(value))
		'LO':return, string(value)
		'LT':return, string(value)
		'PN':return, string(value)
		'SH':return, string(value)
		'ST':return, string(value)
		'TM':return, TM_To_Seconds(string(value))
		'UI':return, string(value)
		'UT':return, string(value)

		;the following are arrays of some sort. For VR = OB,OW,SQ or UN the length can be undefined.

		'AT':return, value
		'OB':return, value
		'OF':return, value
		'OW':return, value
		'UN':return, value
		'SQ':return, value
	endcase
end