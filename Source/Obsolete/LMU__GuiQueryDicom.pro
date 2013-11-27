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


function LMU__GuiQueryDicom, files, first=first,status=Status,pixeldata=pixeldata, Path=Path,event=ev


	;SELECT A FOLDER AND QUERY FILES


	newpath = dialog_pickfile(/directory, path=Path, title = 'Please select the DICOM folder')
	if newpath eq '' then return, 0
	path = newpath

	files = LMU__QueryDicom(path,n,first,pixeldata=pixeldata,status=Status)

	if n eq 0 then begin
		ok = dialog_message(/information,'There are no DICOM data in this folder')
		return, 0
	endif
	if n eq 1 then return, 1

	;PRESENT DETAILS TO THE USER AND ASK FOR SELECTION

	Study 	= LMU__ReadDicom(files[first[0:n-1]],'0008'x,'1030'x)
	Series 	= LMU__ReadDicom(files[first[0:n-1]],'0018'x,'1030'x)
	Patient = LMU__ReadDicom(files[first[0:n-1]],'0010'x,'0010'x)
	Descr = string(Patient) + ', ' + string(Study) + ', ' + string(Series)

	in = PMI__Form(ev.top, Title='Please select which series to load',$
		ptr_new({Type:'LIST', Label:'Series', select:0L,Value:Descr}))
	IF in.cancel THEN BEGIN
		files=''
		return, 0
	ENDIF

	;MAKE A LIST OF ONLY THE SELECTED SERIES


	sel = in.(0)
	nsel = n_elements(in.(0))
	ff = lonarr(nsel+1)
	nf = 0L
	for i=0L,nsel-1 do begin
		ff[i] = nf
		nf = nf + first[sel[i]+1] - first[sel[i]]
	endfor
	ff[nsel] = nf
	f = strarr(nf)
	for i=0L,nsel-1 do f[ff[i]:ff[i+1]-1] = files[first[sel[i]]:first[sel[i]+1]-1]

  	files = f
  	first = ff

	return, 1
end