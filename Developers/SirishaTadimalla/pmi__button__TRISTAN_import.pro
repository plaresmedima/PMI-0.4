
FUNCTION PMI__Button__TRISTAN_Import__QueryFolder, files, first, status=status


;;;;SELECT ONLY DICOM IMAGE DATA AND READ SERIES UID

	n = n_elements(files)
	UID = strarr(n)
	for i=0L,n-1 do begin
		PMI__Message, status, 'Reading DICOM folder', i/(n-1E)
		if PMI__Dicom__Valid(files[i]) then begin
			UID[i] = PMI__Dicom__Read(files[i],'0020'x,'000E'x, ok=ok) ;identify series on series UID
		endif
	endfor

	i = where(UID ne '',n)
	if n eq 0 then begin
		ok = dialog_message(/information,'No DICOM image data in this folder')
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

	return, 1
END


pro PMI__Button__Event__TRISTAN_Import, ev

;;;;SELECT A FOLDER

    PMI__info, ev.top, State=s, Stdy=Stdy, Status=Status
    if not s->get_dir(title='Please select the DICOM folder', files=files, cnt=n) then return
	if n eq 0 then begin
		ok = dialog_message(/information,'This folder is empty')
		return
	endif

	IF NOT PMI__Button__TRISTAN_Import__QueryFolder(files, first, status=status) THEN RETURN

	;; files = an array of strings with DICOM file names, sorted by Series UID
	;; first = an array of indices for the first file of each series

	Manufacturer = PMI__Dicom__Read(files[0],'0008'x,'0070'x)
	Version = PMI__Dicom__Read(files[0],'0008'x,'1090'x)

	Case Manufacturer of
		 'SIEMENS ': Case Version of
		   'Prisma': TRISTAN_Import_Siemens3T, Stdy, files, first, status=status
		   'Aera': TRISTAN_Import_Siemens1_5T, Stdy, files, first, status=status
		   else: ok = dialog_message(/information, Manufacturer + ' Version ' + Version + ' not supported' )
		   endcase
		 'Philips Healthcare': Case Version of
		   'Achieva dStream ': TRISTAN_Import_Philips3T, Stdy, files, first, status=status
		   else: ok = dialog_message(/information, Manufacturer + ' Version ' + Version + ' not supported' )
		   endcase
		 'Philips Medical Systems ': Case Version of
		   'Achieva dStream ': TRISTAN_Import_Philips3T, Stdy, files, first, status=status
		   else: ok = dialog_message(/information, Manufacturer + ' Version ' + Version + ' not supported' )
		   endcase
		 'Philips ': Case Version of
		 	'Ingenia ': TRISTAN_Import_Philips1_5T, Stdy, files, first, status=status
		   else: ok = dialog_message(/information, Manufacturer + ' Version ' + Version + ' not supported' )
		   endcase
		 'Philips Medical Systems ': Case Version of
		 	'Ingenia': TRISTAN_Import_Philips1_5T, Stdy, files, first, status=status
		   else: ok = dialog_message(/information, Manufacturer + ' Version ' + Version + ' not supported' )
		   endcase
		 'GE MEDICAL SYSTEMS': Case Version of
		   'DISCOVERY MR750 ': TRISTAN_Import_GE3T, Stdy, files, first, status=status
		   'DISCOVERY MR450 ': TRISTAN_Import_GE1_5T, Stdy, files, first, status=status
		   else: ok = dialog_message(/information, Manufacturer + ' Version ' + Version + ' not supported' )
		   endcase
		  else: ok = dialog_message(/information, Manufacturer + ' not supported' )
	Endcase


	PMI__control, ev.top, /refresh, Path=Path
	return:PMI__Message, status

end



pro PMI__Button__Control__TRISTAN_Import, id, v

	;PMI__Info, tlb(id), Stdy=Stdy
	widget_control, id, sensitive = 1;obj_valid(Stdy)
end

function PMI__Button__TRISTAN_Import, parent, value=value, separator=separator

	if n_elements(value) eq 0 then value = 'Import DICOM'

	return, widget_button(parent, value=value, separator=separator, $
		event_pro = 'PMI__Button__Event__TRISTAN_Import', $
		pro_set_value = 'PMI__Button__Control__TRISTAN_Import' )

end
