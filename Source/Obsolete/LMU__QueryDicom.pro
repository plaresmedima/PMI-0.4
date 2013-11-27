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


FUNCTION LMU__QueryDicom, path, nSeries, first, pixeldata=pixeldata,status=Status


	PMI__Message, status, 'Reading DICOM folder..'


	nSeries = 0L


	files = FindAllFiles(path,count=n)
	if n eq 0 then return, ''
	UID = strarr(n)

	for i=0L,n-1 do begin

		PMI__Message, status, 'Reading DICOM folder', i/(n-1E)

		if OpenrDicom(files[i],unit) then begin
			free_lun, unit
			if keyword_set(pixeldata) then begin
				Rows = LMU__ReadDicom(files[i],'0028'x,'0010'x)
				if Rows ne 0 then UID[i] = LMU__ReadDicom(files[i],'0020'x,'000E'x)
			endif else UID[i] = LMU__ReadDicom(files[i],'0020'x,'000E'x)
		endif
	endfor


	PMI__Message, status, 'Sorting DICOM folder..'


	i = where(UID ne '',n)
	if n eq 0 then return, ''
	files = files[i]
	UID = UID[i]
	ind = sort(UID)
	UID = UID[ind]
	files = files[ind]




	;COUNT THE NUMBER OF SERIES AND SELECT FIRST FILE OF EACH




	nSeries = n_elements(reduce(UID,first))
	first = [first,n]

	return, files
END
