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


FUNCTION PMI__Dicom__QueryFolder, files, first, Descr=Descr, Imagedata=Imagedata, status=status,no_gui=no_gui


;;;;SELECT ONLY DICOM IMAGE DATA AND READ SERIES UID
	n = n_elements(files)
	UID = strarr(n)
	for i=0L,n-1 do begin
		if n_elements(status) ne 0 then PMI__Message, status, 'Reading DICOM folder', i/(n-1E)
		if PMI__Dicom__Valid(files[i]) then begin
			UIDi = PMI__Dicom__Read(files[i],'0020'x,'000E'x, ok=ok) ;identify series on series UID
			if not ok then UIDi = PMI__Dicom__Read(files[i],'0008'x,'0031'x, ok=ok) ;identify series on series time
			if ok then $
				if keyword_set(imagedata) then Rows = PMI__Dicom__Read(files[i],'0028'x,'0010'x, ok=ok)
			if ok then UID[i] = UIDi
		endif
	endfor
	i = where(UID ne '',n)
	if n eq 0 then begin
		if not keyword_set(no_gui) then ok = dialog_message(/information,'No DICOM image data in this folder')
		return, 0
	endif
	files = files[i]
	UID = UID[i]


;;;;SORT THE SERIES AND SELECT FIRST FILE OF EACH
	ind = sort(UID)
	UID = UID[ind]
	files = files[ind]
	nSeries = n_elements(reduce(UID,first))
	first = [first,n]


	if not arg_present(descr) then return, 1


;;;;GET DESCRIPTION OF THE SERIES
	Descr = strarr(nSeries)
	for i=0L,nSeries-1 do begin
		Study 	= PMI__Dicom__Read(files[first[i]],'0008'x,'1030'x)
		Series 	= PMI__Dicom__Read(files[first[i]],'0018'x,'1030'x)
		SeqName = PMI__Dicom__Read(files[first[i]],'0018'x,'0024'x)
		Patient = PMI__Dicom__Read(files[first[i]],'0010'x,'0010'x)
		Descr[i] = string(Patient) + ', ' + string(Study) + ', ' + string(Series)+ ', ' + string(SeqName)
	endfor

	return, 1
END