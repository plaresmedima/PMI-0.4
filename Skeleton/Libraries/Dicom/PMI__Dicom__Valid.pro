;Checks if a file is DICOM

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


function PMI__Dicom__Valid, file

	on_ioerror, exit

	get_lun, unit
	openr,unit,file
	s = fstat(unit)
	if s.size lt 136 then goto, exit ;132 for the preamble, and at least 4 (gr,el) for the first data element

	point_lun, unit, 128
	preamble = 'DICM'
	readu, unit, preamble

	;if preamble ne 'DICM' then goto, exit

    if preamble eq 'DICM' then begin
        free_lun, unit
	    return, 1B
    endif

    point_lun, unit, 0
    ts = '1.2.840.10008.1.2.1'
    ok = PMI__Dicom__ReadDataElement(unit,'0008'x,'0010'x, value=val, TransferSyntaxUID=ts)
    if not OK then goto, exit
    if strmid(string(val),0,8) ne 'ACR-NEMA' then goto, exit

	free_lun, unit
	return, 1B

	exit:
	free_lun, unit
	return, 0B
end