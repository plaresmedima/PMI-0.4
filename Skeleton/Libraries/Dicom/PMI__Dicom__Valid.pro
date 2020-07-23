;Checks if a file is DICOM


function PMI__Dicom__Valid, file

	on_ioerror, exit

	get_lun, unit
	openr, unit, file
	s = fstat(unit)
	if s.size lt 136 then goto, exit ;132 for the preamble, and at least 4 (gr,el) for the first data element

	point_lun, unit, 128
	preamble = 'DICM'
	readu, unit, preamble
	free_lun, unit

    if preamble eq 'DICM' then return, 1B
    return, PMI__Dicom__CheckNema(file)

	exit:
	free_lun, unit
	return, 0B
end