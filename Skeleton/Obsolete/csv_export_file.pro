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


pro csv_export_file, param, Data, path, Sname, Roiname, excel=excel, proc=proc
	; exports data to csv file
	; first line contains the parameter names and the second line the values
	; keyword excel should be set if you want to import your data to excel spreadsheets

	; Return to caller on error.

	On_Error, 1

   ;dummy variable
	exist=1

   ; Must have data to write.

	IF N_Elements(data) EQ 0 THEN BEGIN
   		Message, 'No Data to save. Returning...', /Informational
   		RETURN
	ENDIF

	;Check if file exists
	sl_pos = STRPOS(path, '\',/REVERSE_SEARCH)
	export_dir = STRMID(path, 0, sl_pos)+'\Export\'
	study = STRMID(path, sl_pos+1, strlen(path))

	;write parameters to textfile syntax is
	; study directory / export
	; filename is study + series + pro thats calling csv_export_file

    filename = study +'_'+ Sname + '_'  + proc + '.csv'
	filename = export_dir  + cleanstr(filename)

	if not file_test(export_dir) then	begin
		FILE_MKDIR, export_dir
	endif

	if not file_test(filename) then begin
		exist = 0
	endif

	xsize = n_elements(data)

	if not exist then begin

		; Check for column header vector.

		IF N_Elements(param) NE 0 THEN BEGIN
	    	length = N_Elements(param)
	    	IF length NE xsize THEN BEGIN
	      		Message, 'Column Header vector wrong size. Returning...', /Informational
	      		RETURN
	    	ENDIF
		ENDIF
	endif

   lineWidth = 1600
   comma = "; "

    ; Open the data file for writing.


	Data = StrTrim(Data,2)

	; replace . by , for excel import

   	if KEYWORD_SET(excel) then Data = excelconvert(Data)

	Data = [Roiname, Data]

   	Data[0:xsize-1] = Data[0:xsize-1] + comma

   	if not exist then begin

		OpenW, lun, filename, /Get_Lun, Width=lineWidth

		IF N_Elements(param) NE 0 THEN BEGIN
			; Write the header to the file

			sColumns = StrTrim(param, 2)

			sColumns = ['Roi', sColumns]

			sColumns[0:xsize-1] = sColumns[0:xsize-1] + comma

			PrintF, lun, sColumns
		ENDIF

	 	; Write the data to the file.

   		PrintF, lun, Data

   		; Close the file.

   		Free_Lun, lun
	endif else begin

		OpenU, lun, filename, /Get_Lun, Width=lineWidth
		;check if region already in file
		rows = file_lines(filename)
		found = 0
		line = ''
		text = strarr(xsize+1,rows)

		;load file to strarray
	   	for i=0, rows -1 do begin
	            readf, lun, line
				test = strsplit(line,';',/extract)
				if strcmp(test(0),roiname) then begin
					text(*,i) = Data
					found = 1
				endif else begin
					test(0:xsize-1) = test(0:xsize-1) + comma
					text(*,i)=test(*)
				endelse
	    endfor
		free_lun, lun
		;;end of check

		;finally write data
		openw, unit, filename, /get_lun, Width=lineWidth

		for i=0l, rows -1 do printf, unit, text(*,i)
		if not found then printf, unit, Data

    	free_lun, unit

	endelse





end