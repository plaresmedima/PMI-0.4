
FUNCTION TRISTAN_QueryFolder, files, first, status=status


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


pro TRISTAN_Batch_Import, path, studyname

	PMI__info, ev.top, state=State, Viewer=Viewer
	Stdy = obj_new('STDY_STATE',path+studyname)
	Stdy -> SaveStdy
	State -> Insert, Stdy
	PMI__Button__RecentStudy__Build, ev.top, /update

    files = file_search(path,'*', /TEST_REGULAR, count=n)
	IF NOT TRISTAN_QueryFolder(files, first, status=status) THEN RETURN

;; files = an array of strings with DICOM file names, sorted by Series UID
;; first = an array of indices for the first file of each series

	Manufacturer = PMI__Dicom__Read(files[0],'0008'x,'0070'x)
	Version = PMI__Dicom__Read(files[0],'0008'x,'1090'x)

	Case Manufacturer of
		 'SIEMENS ': Case Version of
		   'Prisma': TRISTAN_Import_Siemens3T, Stdy, files, first, status=status
		   'Aera': ;TRISTAN_Import_Siemens1_5T, Stdy, files, first, status=status
		   else: ok = dialog_message(/information, Manufacturer + ' Version ' + Version + ' not supported' )
		   endcase
		 'Philips Medical Systems': Case Version of
		   'Achieva dStream': ;TRISTAN_Import_Philips3T, Stdy, files, first, status=status
		   'Ingenia': ;TRISTAN_Import_Philips1_5T, Stdy, files, first, status=status
		    else: ok = dialog_message(/information, Manufacturer + ' Version ' + Version + ' not supported' )
		  endcase
		 'Bordeaux': Case Version of
		   'BEAT-DKD_10_1': ;TRISTAN_Import_Bordeaux_10_1, Stdy, files, first, status=status
		   'BEAT-DKD_10_2': ;TRISTAN_Import_Bordeaux_10_2, Stdy, files, first, status=status
		    else: ok = dialog_message(/information, Manufacturer + ' Version ' + Version + ' not supported' )
		    endcase
		  else: ok = dialog_message(/information, Manufacturer + ' not supported' )
	Endcase
	Stdy -> SaveStdy

end
