;Reads the length field
;File pointer must be placed immediately before the length field


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


FUNCTION PMI__Dicom__ReadLength, unit, gr, ts, vr

	implicit_vr = (gr NE '0002'x) AND (ts EQ '1.2.840.10008.1.2')
    IF implicit_vr THEN BEGIN
    	length = BYTARR(4)
    	READU, unit, length
    	RETURN, ULONG(length,0,1)
    ENDIF

	;EXPLICIT VR

	big_endian = (gr NE '0002'x) AND (ts EQ '1.2.840.10008.1.2.2')
	long = TOTAL(vr EQ ['UT','OB','OF','OW','UN','SQ'], /PRESERVE_TYPE)
	IF long THEN length = BYTARR(4) ELSE length = BYTARR(2)
	READU, unit, length
	IF big_endian THEN length = REVERSE(length,/OVERWRITE)
	IF long THEN length = ULONG(length,0,1) ELSE length = UINT(length,0,1)
	RETURN, length
END