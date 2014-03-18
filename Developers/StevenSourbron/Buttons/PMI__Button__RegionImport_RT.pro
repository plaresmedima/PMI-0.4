pro PMI__Button__Event__RegionImport_RT, ev

    PMI__info, ev.top, State=s, Stdy=Stdy, Status=Status, Series=Series

	if not s -> get_file(file, title= 'Select contour file', filter= '.dcm') then return

	RoiContourSequence = PMI__Dicom__Read(file, '3006'x, '0039'x, ok=valid)
	IF NOT valid THEN BEGIN
    	ok = dialog_message(/information,'This is not a valid DICOM file')
    	return
	ENDIF

	ContourSequence = RoiContourSequence[0]->GetValue('3006'x, '0040'x, error=error)
	IF error THEN BEGIN
    	ok = dialog_message(/information,'No contour data in this file')
    	return
	ENDIF

	PixelSpacing = Series -> GetValue('0028'x,'0030'x)
	ImagePosition = Series -> GetValue('0020'x,'0032'x)
	ImagePosition = ImagePosition[0:1]
	SliceDimensions = Series->d()
	SliceLocations = Series->z()

	Region = Stdy->New('REGION', Name='RT CONTOUR', Domain=Series->dom(), Color=Series->Clr(SAT='R'))

	nContours = n_elements(ContourSequence)
	for i=0L,nContours-1 do begin
		ContourData = ContourSequence[i] -> GetValue('3006'x, '0050'x, error=error)
		ContourSlice = ContourData[2]
		Mask = PMI__Dicom__Contour(ContourData, ImagePosition, PixelSpacing, SliceDimensions)
		SliceIndex = where(ContourSlice eq SliceLocations, cnt)
		if cnt GT 0 THEN BEGIN
			Mask = Mask OR Region -> read(Stdy->DataPath(), SliceIndex[0])
			Region -> Write, Stdy->DataPath(), Mask, SliceIndex[0]
		ENDIF
	endfor

    PMI__control, ev.top, /refresh
end


pro PMI__Button__Control__RegionImport_RT, id, v

	PMI__Info, tlb(id), Series=Series
	widget_control, id, sensitive = obj_valid(Series)
end

function PMI__Button__RegionImport_RT, parent, value=value, separator=separator

	if n_elements(value) eq 0 then value = 'Import radiotherapy countours'

    return, widget_button(parent,  $
      	value      		= value, separator=separator,  $
      	event_pro    	= 'PMI__Button__Event__RegionImport_RT', $
    	pro_set_value 	= 'PMI__Button__Control__RegionImport_RT' )
end