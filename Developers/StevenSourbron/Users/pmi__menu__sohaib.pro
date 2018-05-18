pro PMI__Menu__Sohaib, parent

	PMI__Menu__Skeleton, parent
	PMI__Menu__Slices, parent
	PMI__Menu__Dynamic, parent

	id = widget_button(parent, value='Sohaib',/menu)

	Sid = PMI__Button__SohaibDeconvolution(id,value='Map: Model-free')
	Sid = PMI__Button__SohaibCompartmentLinear(id,value='Map: One compartment')
	Sid = PMI__Button__SohaibCompartmentLinearDelay(id,value='Map: One compartment + delay')
	Sid = PMI__Button__SohaibFermiDeconvolution(id,value='Map: Fermi model')

	Sid = PMI__Button__CardiacSingleInletRoiSlider(id, value='ROI: Model-based', /separator)
	Sid = PMI__Button__SeriesExportDicom(id, value='DICOM export', /separator)

end