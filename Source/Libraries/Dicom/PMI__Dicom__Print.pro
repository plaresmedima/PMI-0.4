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


function PMI__Dicom__Print, file, n

	if not PMI__Dicom__Openr(file, read, TransferSyntaxUID=TS) then return, 0B

	ImplicitVR = TS eq '1.2.840.10008.1.2'
	If ImplicitVR then Hdr=LMU__DicomTemplate()
	if n_params() eq 2 then begin
		FOR i=0L, n-1 DO BEGIN
			de = obj_new('DATA_ELEMENT',unit=read, TransferSyntaxUID=TS, Template=Hdr)
			print, i, de->text()
		ENDFOR
	endif else begin
		while not eof(read) do begin
			de = obj_new('DATA_ELEMENT',unit=read, TransferSyntaxUID=TS, Template=Hdr)
			print, de->text()
		endwhile
	endelse

	If ImplicitVR then obj_destroy, Hdr
	close, read
	free_lun, read

	return, 1B
end