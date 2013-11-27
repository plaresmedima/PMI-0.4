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


function PMI__Dicom__TextToValue__TextToArray, v

	Array = strsplit(v,',',/extract,count=cnt)
	if cnt gt 1 then begin
		Array[0] = strmid(Array[0],1)
		Array[cnt-1] = strmid(Array[cnt-1],0,strlen(Array[cnt-1])-1)
	endif else Array=Array[0]
	return, Array
end


;Converts a text value into a formatted DICOM value
;i.e. This inverts PMI__DICOM__TEXTVALUE()

function PMI__Dicom__TextToValue, v, vr

	case vr of

		;Sequence

		'SQ':return, byte(strsplit(v,';'))

		;these are strings

		'AE':return, v
		'AS':return, v
		'CS':return, v
		'DA':return, v
		'DT':return, v
		'LO':return, v
		'LT':return, v
		'PN':return, v
		'SH':return, v
		'ST':return, v
		'UI':return, v
		'UT':return, v

		;these are the numbers or arrays of numbers

		'DS':return, double(PMI__Dicom__TextToValue__TextToArray(v))
		'IS':return, long(PMI__Dicom__TextToValue__TextToArray(v))
		'TM':return, double(PMI__Dicom__TextToValue__TextToArray(v))
	 	'FL':return, float(PMI__Dicom__TextToValue__TextToArray(v))
		'FD':return, double(PMI__Dicom__TextToValue__TextToArray(v))
		'SL':return, long(PMI__Dicom__TextToValue__TextToArray(v))
		'SS':return, fix(PMI__Dicom__TextToValue__TextToArray(v))
		'UL':return, ulong(PMI__Dicom__TextToValue__TextToArray(v))
		'US':return, uint(PMI__Dicom__TextToValue__TextToArray(v))
		'AT':return, uint(PMI__Dicom__TextToValue__TextToArray(v))
		'OB':return, byte(PMI__Dicom__TextToValue__TextToArray(v))
		'OF':return, float(PMI__Dicom__TextToValue__TextToArray(v))
		'OW':return, uint(PMI__Dicom__TextToValue__TextToArray(v))
		'UN':return, byte(PMI__Dicom__TextToValue__TextToArray(v))

		;If the vr is invalid

		else:return, byte(v)

	endcase


end