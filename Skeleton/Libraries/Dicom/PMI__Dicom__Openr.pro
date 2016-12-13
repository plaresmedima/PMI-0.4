;Opens a Dicom file for reading
;Returns 0 if the file is not dicom, else returns 1
;If the file is dicom, the file pointer is returned in unit
;Transfer syntax UID is read
;and the file pointer is positioned just after the preamble.



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


function PMI__Dicom__Openr, file, unit, TransferSyntaxUID=ts

	on_ioerror, exit

	get_lun, unit
	openr,unit,file
	s = fstat(unit)
	if s.size lt 136 then goto, exit ;132 for the preamble, and at least 4 (gr,el) for the first data element

	point_lun, unit, 128
	preamble = 'DICM'
	readu, unit, preamble

    if preamble eq 'DICM' then begin
        point_lun, -unit, p
	    ok = PMI__Dicom__ReadDataElement(unit,'0002'x,'0010'x, value=ts, TransferSyntaxUID='1.2.840.10008.1.2.1')
	    if not OK then goto, exit
	    point_lun, unit, p
	    return, 1B
    endif

    free_lun, unit
    if PMI__Dicom__CheckNema(file) then begin
        get_lun, unit
 	    openr,unit,file
 	    ts = '1.2.840.10008.1.2.1'
 	    return, 1B
    endif

	exit:
	free_lun, unit
	return, 0B
end