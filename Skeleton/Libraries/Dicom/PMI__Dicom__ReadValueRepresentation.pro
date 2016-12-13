;Reads the value representation (VR) for Explicit VR Transfer Syntax
;File pointer must be placed immediately before the VR field

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


FUNCTION PMI__Dicom__ReadValueRepresentation, unit

    vr = bytarr(2)
    readu, unit, vr
    vr = string(vr)

    skip = total(vr EQ ['UT','OB','OF','OW','UN','SQ'],/PRESERVE_TYPE)
    IF skip THEN BEGIN
    	point_lun, -unit, p
    	point_lun, unit, p+2
	ENDIF

	return, vr
END