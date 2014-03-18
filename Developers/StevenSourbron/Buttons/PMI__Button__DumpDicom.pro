pro PMI__Button__Event__DumpDicom, ev

    PMI__info, ev.top, State=s, Status=Status
    if not s->get_dir(Path, title='Please select the DICOM folder', files=files, cnt=n) then return
	if n eq 0 then begin
		ok = dialog_message(/information,'Empty folder')
		return
	endif

	IF NOT PMI__Dicom__QueryFolder(files, first, status=status) THEN RETURN

	in = PMI__Form(ev.top, Title='Input for DICOM dump', [$
		ptr_new({Type:'DROPLIST',Tag:'sel', Label:'Write header for', Value:[ 'One image per series','All images' ], Select:0L})])
	IF in.cancel THEN RETURN

	Path = Path + '\DicomHeaders'
	file_mkdir, Path

	CASE in.sel OF
		0:indices = first[0:n_elements(first)-2]
		1:indices = lindgen(n_elements(files))
	ENDCASE

	n = n_elements(indices)
	FOR i=0L,n-1 DO BEGIN
		PMI__Message, status, 'Writing File ', i/(n-1.0)
		file = files[indices[i]]
		ok = PMI__Dicom__Dump(file, Path + '\' + fname(file) + '.txt')
	ENDFOR

	PMI__Message, status
end


pro PMI__Button__Control__DumpDicom, id, v
	widget_control, id, sensitive = 1
end


function PMI__Button__DumpDicom, parent,separator=separator

	return, widget_button(parent, separator	= separator, $
  		value = 'Dump DICOM header', $
  		event_pro = 'PMI__Button__Event__DumpDicom', $
  		pro_set_value = 'PMI__Button__Control__DumpDicom' )

end
