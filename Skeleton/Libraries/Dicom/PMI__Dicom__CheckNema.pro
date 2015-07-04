pro PMI__Dicom__CheckNema__Test

file = dialog_pickfile() ;ACR-NEMA
print, file
print, PMI__Dicom__CheckNema(file)

end

function PMI__Dicom__CheckNema, file

    get_lun, unit
    openr, unit, file
    s = fstat(unit)
    length = 0UL

    while 1 do begin
        point_lun, -unit, p
        if s.size-p lt 8 then goto, exit
        point_lun, unit, p+4
        readu, unit, length
        point_lun, -unit, p
        if s.size-p lt length then goto, exit
        if length gt 0UL then begin
            if length gt 2UL^20UL then goto, exit
            value = bytarr(length)
            readu, unit, value
            if length ge 8 then $
                if string(value[0:7]) eq 'ACR-NEMA' then begin
                   free_lun, unit
                   return, 1B
                endif
        endif
        if eof(unit) then goto, exit
    endwhile

	exit:
	free_lun, unit
	return, 0B
end