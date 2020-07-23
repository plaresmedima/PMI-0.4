;Returns the value of data element with tags "group" and "element"




function PMI__Dicom__Read, file, group, element, ok=ok

	n = n_elements(file)
	if n gt 1 then begin
		v0 = PMI__Dicom__Read(file[0], group, element, ok=ok)
		if not ok then return, bytarr(n)
		value = make_array(n,type=size(v0,/type))
		value[0] = v0
		for i=1L,n-1 do value[i] = PMI__Dicom__Read(file[i], group, element)
		return, value
	endif

	ok = 0B
	if not PMI__Dicom__Openr(file, unit, TransferSyntaxUID=ts) then return, 0B
	implicit_vr = (group NE '0002'x) AND (ts EQ '1.2.840.10008.1.2')
	If implicit_vr then Hdr=LMU__DicomTemplate()
	ok = PMI__Dicom__ReadDataElement(unit, group, element, value=value, TransferSyntaxUID=ts, Template=Hdr)
	If implicit_vr then obj_destroy, Hdr
	free_lun, unit
	return, value
end