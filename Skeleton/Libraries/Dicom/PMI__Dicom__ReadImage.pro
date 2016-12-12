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


function PMI__Dicom__ReadImage, file, ok=ok, Info=Info

	if not PMI__Dicom__Openr(file,unit, TransferSyntaxUID=TS) then return, 0B

	If TS eq '1.2.840.10008.1.2' then Hdr=LMU__DicomTemplate()
	ok = PMI__Dicom__ReadDataElement(unit,'0008'x,'0070'x,value=Manufacturer, TransferSyntaxUID=TS, Template=Hdr)
	Philips = Manufacturer Eq 'Philips Medical Systems '

	if arg_present(info) then begin
		Info = Obj_new('HEADER')
		Info->Set, Obj_new('DATA_ELEMENT','0020'x,'0032'x, unit=unit, TransferSyntaxUID=TS, Template=Hdr)
		Info->Set, Obj_new('DATA_ELEMENT','0020'x,'0037'x, unit=unit, TransferSyntaxUID=TS, Template=Hdr), /append
		Info->Set, Obj_new('DATA_ELEMENT','0028'x,'0030'x, unit=unit, TransferSyntaxUID=TS, Template=Hdr), /append
	endif

	ok = PMI__Dicom__ReadDataElement(unit,'0028'x,'0010'x,value=ny, TransferSyntaxUID=TS, Template=Hdr)

	if not ok then begin
		free_lun, unit
		If obj_valid(Hdr) then obj_destroy, Hdr
		return,0B
	endif

	ok = PMI__Dicom__ReadDataElement(unit,'0028'x,'0011'x,value=nx, TransferSyntaxUID=TS, Template=Hdr)

	if Philips then begin
		ok = PMI__Dicom__ReadDataElement(unit,'2005'x,'100D'x,value=b, TransferSyntaxUID=TS, Template=Hdr)
		ok = PMI__Dicom__ReadDataElement(unit,'2005'x,'100E'x,value=a, TransferSyntaxUID=TS, Template=Hdr)
		if n_elements(a) gt 1 then a=1E
		if n_elements(b) gt 1 then b=0E
	endif else begin
		ok = PMI__Dicom__ReadDataElement(unit,'0028'x,'1052'x,value=b, TransferSyntaxUID=TS, Template=Hdr)
		ok = PMI__Dicom__ReadDataElement(unit,'0028'x,'1053'x,value=a, TransferSyntaxUID=TS, Template=Hdr)
	endelse

	ok = PMI__Dicom__ReadDataElement(unit,'7FE0'x,'0010'x,value=im, TransferSyntaxUID=TS, Template=Hdr)

	free_lun, unit
	If obj_valid(Hdr) then obj_destroy, Hdr
	if not ok then return, 0B

	n = long(nx)*ny
	if n_elements(im) gt n then im=im[0:n-1]
	if n_elements(im) eq n then begin
		im = reform(im,nx,ny,/overwrite)
		im = reverse(im,2,/overwrite)
		if a ne 0 then begin
			if Philips then im = (float(im) - float(b))/float(a) $
			else im = float(a)*float(im) + float(b)
		endif
	endif else im = fltarr(nx,ny)

	return, im
end