
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


pro PMI__Button__Event__TRISTAN_Batch_Import, ev

	PMI__info, ev.top, State=State, Stdy=Stdy, Status=Status
	if not State -> get_file(batchfile, title= 'Select batch file', filter= '.xml') then return

	; Load and read XML file
	XMLDocument = OBJ_NEW('IDLffXMLDOMDocument', FILENAME=batchfile)
	XMLRoot = XMLDocument->GetDocumentElement()
	scanner = XMLRoot->GetAttribute('type')

	XMLStudy = XMLDocument->GetFirstChild()
	oTags = XMLStudy->GetElementsByTagName('path')

	; Number of datasets to process
	N = oTags->GetLength()

	; Loop through each dataset from the XML batch file
	for k=0, N-1 do begin

		; Get study path and PMI destination filename
		oPath = oTags->Item(k)
		oPathText = oPath->GetFirstChild()
		studypath = oPathText->GetNodeValue()
		oTags = XMLStudy->GetElementsByTagName('name')
		oName = oTags->Item(0)
		oNameText = oName->GetFirstChild()
		studyname = oNameText->GetNodeValue()

		; Create a new study
		Stdy = obj_new('STDY_STATE',studypath+'\'+studyname)
		Stdy -> SaveStdy

		State -> Insert, Stdy
		PMI__Button__RecentStudy__Build, ev.top, /update

		; Browse through all files within study folder
    	files = file_search(studypath+'\','*', /TEST_REGULAR, count=n)
		IF NOT PMI__Button__TRISTAN_Import__QueryFolder(files, first, status=status) THEN RETURN

		Case scanner of
			 'SIEMENS 3T': TRISTAN_Import_Siemens3T, Stdy, files, first, status=status
		   	 'SIEMENS 1.5T': ;TRISTAN_Import_Siemens1_5T, Stdy, files, first, status=status
		 	 'PHILIPS 3T': ;TRISTAN_Import_Philips3T, Stdy, files, first, status=status
		     'PHILIPS 1.5T': ;TRISTAN_Import_Philips1_5T, Stdy, files, first, status=status
		     'GE 3T': ;TRISTAN_Import_Bordeaux_10_1, Stdy, files, first, status=status
		     'GE 1.5T': ;TRISTAN_Import_Bordeaux_10_2, Stdy, files, first, status=status
		     else: ok = dialog_message(/information, scanner + ' not supported' )
		Endcase
		Stdy -> SaveStdy
	endfor
end

pro PMI__Button__Control__TRISTAN_Batch_Import, id, v

	;PMI__Info, tlb(id), Stdy=Stdy
	widget_control, id, sensitive = 1;obj_valid(Stdy)
end

function PMI__Button__TRISTAN_Batch_Import, parent, value=value, separator=separator

	if n_elements(value) eq 0 then value = 'Import DICOM'

	return, widget_button(parent, value=value, separator=separator, $
		event_pro = 'PMI__Button__Event__TRISTAN_Batch_Import', $
		pro_set_value = 'PMI__Button__Control__TRISTAN_Batch_Import' )

end
