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


function PMI__Dicom__TextValue__ArrayToText, v

	n = n_elements(v)
	if n ge 100 then return, strcompress(n,/remove_all) + '-element array (not printed)'
	vs = strcompress(v,/remove_all)
	n = n_elements(vs)
	if n eq 1 then return, vs[0]
	s='[' + vs[0]
	for i=1L,n-1 do s=s+','+vs[i]
	return, s + ']'

end

;Converts a formatted DICOM value into a text value
;Inverted by PMI__DICOM__TEXTTOVALUE()

function PMI__Dicom__TextValue, v, vr

	case vr of

		;Sequence

		'SQ':begin
			if not obj_valid(v[0]) then n=0L else n=n_elements(v)
			return, 'SEQUENCE ('+strcompress(n,/remove_all) + ' DATA SETS)'
			end

		;these are strings

		'AE':return, string(v)
		'AS':return, string(v)
		'CS':return, string(v)
		'DA':return, string(v)
		'DT':return, string(v)
		'LO':return, string(v)
		'LT':return, string(v)
		'PN':return, string(v)
		'SH':return, string(v)
		'ST':return, string(v)
		'UI':return, string(v)
		'UT':return, string(v)

		;these are the numbers or arrays of numbers

		'DS':return, PMI__Dicom__TextValue__ArrayToText(v)
		'IS':return, PMI__Dicom__TextValue__ArrayToText(v)
		'TM':return, PMI__Dicom__TextValue__ArrayToText(v)
	 	'FL':return, PMI__Dicom__TextValue__ArrayToText(v)
		'FD':return, PMI__Dicom__TextValue__ArrayToText(v)
		'SL':return, PMI__Dicom__TextValue__ArrayToText(v)
		'SS':return, PMI__Dicom__TextValue__ArrayToText(v)
		'UL':return, PMI__Dicom__TextValue__ArrayToText(v)
		'US':return, PMI__Dicom__TextValue__ArrayToText(v)
		'AT':return, PMI__Dicom__TextValue__ArrayToText(v)
		'OB':return, PMI__Dicom__TextValue__ArrayToText(v)
		'OF':return, PMI__Dicom__TextValue__ArrayToText(v)
		'OW':return, PMI__Dicom__TextValue__ArrayToText(v)
		'UN':return, PMI__Dicom__TextValue__ArrayToText(v)

		;If the vr is invalid

		else: begin
			if n_elements(v) ne 0 then return, string(v)
			return, 'XX'
			end

	endcase

end