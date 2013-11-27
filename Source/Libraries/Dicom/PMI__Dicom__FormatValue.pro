;Upon input value is a byte-array.

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


FUNCTION PMI__Dicom__FormatValue, value, vr, length, big_endian = big_endian

	CASE vr OF

	 	;these are the numbers (can be arrays if the value multiplicity is larger than 1)

	 	'FL':BEGIN
	 		value = reform(value,4,length/4,/overwrite)
	 		IF KEYWORD_SET(big_endian) THEN value=reverse(value,1,/overwrite)
	 		value = float(value,0,length/4)
	 		IF length/4 EQ 1 THEN value=value[0]
	 		END
		'FD':BEGIN
	 		value = reform(value,8,length/8,/overwrite)
	 		IF KEYWORD_SET(big_endian) THEN value=reverse(value,1,/overwrite)
	 		value = double(value,0,length/8)
	 		IF length/8 EQ 1 THEN value=value[0]
	 		END
		'SL':BEGIN
	 		value = reform(value,4,length/4,/overwrite)
	 		IF KEYWORD_SET(big_endian) THEN value=reverse(value,1,/overwrite)
	 		value = long(value,0,length/4)
	 		IF length/4 EQ 1 THEN value=value[0]
	 		END
		'SS':BEGIN
	 		value = reform(value,2,length/2,/overwrite)
	 		IF KEYWORD_SET(big_endian) THEN value=reverse(value,1,/overwrite)
	 		value = fix(value,0,length/2)
	 		IF length/2 EQ 1 THEN value=value[0]
	 		END
		'UL':BEGIN
	 		value = reform(value,4,length/4,/overwrite)
	 		IF KEYWORD_SET(big_endian) THEN value=reverse(value,1,/overwrite)
	 		value = ulong(value,0,length/4)
	 		IF length/4 EQ 1 THEN value=value[0]
	 		END
		'US':BEGIN
	 		value = reform(value,2,length/2,/overwrite)
	 		IF KEYWORD_SET(big_endian) THEN value=reverse(value,1,/overwrite)
	 		value = uint(value,0,length/2)
	 		IF length/2 EQ 1 THEN value=value[0]
	 		END

		;the following are arrays of some sort.

		'AT':begin
			value = reform(value,2,length/2,/overwrite)
			IF KEYWORD_SET(big_endian) THEN value=reverse(value,1,/overwrite)
			value = uint(value,0,length/2)
	 		IF length/2 EQ 1 THEN value=value[0]
			end
		'OB':IF length EQ 1 THEN value=value[0]
		'OF':BEGIN
	 		value = reform(value,4,length/4,/overwrite)
	 		IF KEYWORD_SET(big_endian) THEN value=reverse(value,1,/overwrite)
	 		value = float(value,0,length/4)
	 		IF length/4 EQ 1 THEN value=value[0]
	 		END
		'OW':BEGIN
	 		value = reform(value,2,length/2,/overwrite)
	 		IF KEYWORD_SET(big_endian) THEN value=reverse(value,1,/overwrite)
	 		value = fix(value,0,length/2)
	 		IF length/2 EQ 1 THEN value=value[0]
	 		END

		;these are strings that represent values. When value multiplicity is larger than 1, they are returned as arrays

		'DS':begin
			value = double(strsplit(string(value),'\',/extract,count=cnt))
			if cnt eq 1 then value=value[0]
			end
		'IS':begin
			value = long(strsplit(string(value),'\',/extract,count=cnt))
			if cnt eq 1 then value=value[0]
			end
		'TM':begin
			value = TM_To_Seconds(strsplit(string(value),'\',/extract,count=cnt))
			if cnt eq 1 then value=value[0]
			end

		;these are strings

 		'AE':value = string(value)
		'AS':value = string(value)
		'CS':value = string(value)
		'DA':value = string(value)
		'DT':value = string(value)
		'LO':value = string(value)
		'LT':value = string(value)
		'PN':value = string(value)
		'SH':value = string(value)
		'ST':value = string(value)
		'UI':value = string(value)
		'UT':value = string(value)

		'UN':;Remains unformatted
		'SQ':;Formatted upon loading
		else:
	ENDCASE

	return, value
END