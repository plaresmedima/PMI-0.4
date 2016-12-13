pro PMI__Menu__Arshad, parent

	PMI__Menu__Skeleton, parent
	PMI__Menu__Slices, parent

	id = widget_button(parent, value='Arshad',/menu)

	Sid = PMI__Button__SlicesResolution(id,value='Interpolate images')
	Sid = PMI__Button__ExtractTimeWindow(id,value='Remove motion')
	Sid = PMI__Button__ArshadDeconvolution(id,value='Quantitative analysis (Pixel)',/separator)
	Sid = PMI__Button__ArshadDeconvolutionRoi(id,value='Quantitative analysis (ROI)')
	Sid = PMI__Button__ArshadFermiRoi(id,value='Fermi model analysis (ROI)')
	Sid = PMI__Button__ArshadFermiRoiStressRest(id,value='T1-corrected Fermi model analysis (ROI)')
	Sid = PMI__Button__RegionSelectByRangeInteractive(id,/separator)
	Sid = PMI__Button__SlicesSmooth(id, value='Uniform slice smoothing')
	Sid = PMI__Button__DynamicSmooth(id, value='Uniform curve smoothing')
	Sid = PMI__Button__DumpDicom(id,/separator)

end
