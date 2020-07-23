
FUNCTION PMI__Button__iBEAt_Import__ExtractDICOM, status, files

    n = n_elements(files)
	dicom = bytarr(n)
	for i=0L,n-1 do begin
	  PMI__Message, status, 'Extracting DICOM data', i/(n-1E)
	  dicom[i] = PMI__Dicom__Valid(files[i])
	endfor
	i = where(dicom EQ 1B, n)
	if n eq 0 then begin
		ok = dialog_message(/information,'No DICOM image data in this folder')
		RETURN, 0B
	endif
	files = files[i]
	RETURN, 1B
END




FUNCTION PMI__Button__iBEAt_Import__Version, file, site

	IF site EQ 'TURKU PET centre' THEN BEGIN
	  RETURN, PMI__Dicom__Read(file,'2001'x,'10C8'x)
	ENDIF

	IF site EQ 'Leeds AIC' THEN BEGIN
	  RETURN, 'BEAT-DKD_10_6'
	ENDIF
END






FUNCTION PMI__Button__iBEAt_Import__Site, file

	Site = PMI__Dicom__Read(file,'0008'x,'0080'x)
	IF Site NE 0B THEN RETURN, site ;Turku

	value = PMI__Dicom__Read(file,'0040'x,'0254'x)
	IF value EQ 'Abdominal^SS BEAT Kidney' THEN RETURN, 'Leeds AIC'

	RETURN, 1B
END






PRO PMI__Button__Event__iBEAt_Import, ev

;Select a folder and get all DICOM files in the folder

    PMI__info, ev.top, State=s, Stdy=Stdy, Status=Status
    IF NOT s->get_dir(title='Please select the DICOM folder', files=files, cnt=n) THEN RETURN
	IF n EQ 0 THEN BEGIN
		ok = dialog_message(/information,'This folder is empty')
		RETURN
	ENDIF
    IF NOT PMI__Button__iBEAt_Import__ExtractDICOM(status, files) THEN GOTO, RETURN

;Identify site and version and launch import script

	Site = PMI__Button__iBEAt_Import__Site(files[0])
	Version = PMI__Button__iBEAt_Import__Version(files[0], Site)

	CASE Site OF

		'TURKU PET centre': Case Version of
		 ;  'BEAT_DKD_10.8 ': iBEAt_Import_Turku_10_8, Stdy, files, status=status
		   else: ok = dialog_message(/information, Site + ' Version number ' + Version + ' not yet supported' )
		   endcase

		 'Leeds AIC': Case Version of
		   'BEAT-DKD_10_6': iBEAt_Import_Leeds_10_6, Stdy, files, status=status
		    else: ok = dialog_message(/information, Site + ' Version number ' + Version + ' not yet supported' )
		    endcase

		  ELSE: ok = dialog_message(/information, Site + ' not yet supported' )

	ENDCASE

	PMI__control, ev.top, /refresh, Path=Path
	return:PMI__Message, status

END






pro PMI__Button__Control__iBEAt_Import, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	widget_control, id, sensitive = obj_valid(Stdy)
end

function PMI__Button__iBEAt_Import, parent, value=value, separator=separator

	if n_elements(value) eq 0 then value = 'Import DICOM'

	return, widget_button(parent, value=value, separator=separator, $
		event_pro = 'PMI__Button__Event__iBEAt_Import', $
		pro_set_value = 'PMI__Button__Control__iBEAt_Import' )

end
