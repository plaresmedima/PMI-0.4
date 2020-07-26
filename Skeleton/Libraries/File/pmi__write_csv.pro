

;Writes a 1- or 2-dimensional string array to file in comma-separated-value format

pro PMI__write_csv, file, array, ERROR=Err

	Openw, 1, file, ERROR=Err
	If Err ne 0 then return

	n = size(array,/n_dimensions)
	d = size(array,/dimensions)

	if n eq 1 then begin
		for i=0L,d[0]-1 do printf, 1, array[i]
	endif else begin
		for i=0L,d[1]-1 do begin
			str = array[0,i]
			for j=1L, d[0]-1 do str=str + ','+array[j,i]
			printf, 1, str
		endfor
	endelse

	Close, 1
end