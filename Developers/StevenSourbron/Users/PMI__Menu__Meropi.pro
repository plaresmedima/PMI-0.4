pro PMI__Menu__Meropi, parent

	PMI__Menu__Study, parent
	PMI__Menu__Series, parent
	PMI__Menu__Region, parent
	PMI__Menu__Window, parent
	PMI__Menu__Slices, parent
	PMI__Menu__Dynamic, parent

	id = widget_button(parent, value='Meropi',/menu)

	Sid = PMI__Button__SemiQuantitativePerfusion(id	, value='Semi-quantitative (Pixel)')
	Sid = PMI__Button__FastDeconvolutionAnalysis(id, value='Deconvolution (Pixel)')
	Sid = PMI__Button__KidneyModelsROI(id, value='Kidney Models (ROI)')
	Sid = PMI__Button__ExtractTimeWindow(id, value='Extract ROI window',/separator)

end
