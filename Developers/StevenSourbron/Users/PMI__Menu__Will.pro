pro PMI__Menu__Will, parent

	PMI__Menu__Skeleton, parent
	PMI__Menu__Slices, parent
	PMI__Menu__Dynamic, parent
	PMI__Menu__Perfusion, parent

	id = widget_button(parent, value='Will',/menu)

	Sid = PMI__Button__SlicesResolution(id,value='Interpolate images')
	Sid = PMI__Button__ExtractTimeWindow(id,value='Remove motion')
	Sid = PMI__Button__ArshadDeconvolution(id,value='Quantitative analysis (Pixel)',/separator)
	Sid = PMI__Button__ArshadDeconvolutionRoi(id,value='Quantitative analysis (ROI)')
	Sid = PMI__Button__RegionSelectByRangeInteractive(id,/separator)
	Sid = PMI__Button__DumpDicom(id,/separator)

end
