pro PMI__Menu__Marica, parent

	PMI__Menu__Skeleton, parent

	id = widget_button(parent, value='Marica',/menu)

	Sid = PMI__Button__SemiQuantitativePerfusion(id	, value='Descriptive (Pixel)')
	Sid = PMI__Button__FastDeconvolutionAnalysis(id, value='Deconvolution (Pixel)')
	Sid = PMI__Button__KidneyModelsROI(id, value='ROI-based analysis')

end
