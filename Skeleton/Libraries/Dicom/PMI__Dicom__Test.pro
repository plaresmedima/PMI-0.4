;Writes a text file with the DICOM header data
;Returns 1 when writing is completed without error, and 0 otherwise


;
;    Copyright (C) 2015 Steven Sourbron
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


function PMI__Dicom__Test, file

	if not PMI__Dicom__Openr(file, unit, TransferSyntaxUID=TS) then return, 0B

    s = fstat(unit)

	Hdr = LMU__DicomTemplate()
	while not eof(unit) do begin
	    ok = PMI__Dicom__ReadDataElement(unit, gr=gr, el=el, vr=vr, value=v, length=length, TransferSyntaxUID=TS, Template=Hdr)
	    print, gr
	    print, el
	    print, vr
	    print, length
	    print, ''
	endwhile
	obj_destroy, Hdr

	close, unit & free_lun, unit

	return, 1B

end
