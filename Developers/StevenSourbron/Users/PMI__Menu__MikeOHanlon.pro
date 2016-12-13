pro PMI__Menu__MikeOHanlon, parent

	PMI__Menu__Study, parent
	PMI__Menu__Series, parent
	PMI__Menu__Region, parent
	PMI__Menu__Window, parent
	PMI__Menu__Slices, parent
	PMI__Menu__Dynamic, parent

	id = widget_button(parent, value='Mike',/menu)

	Sid = PMI__Button__SemiQuantitativePerfusion(id	, value='Semi-quantitative (Pixel)')
	Sid = PMI__Button__SemiQuantitativePerfusionRoi(id, value='Semi-quantitative (ROI)')
	Sid = PMI__Button__FastDeconvolutionAnalysis(id, value='Deconvolution (Pixel)', /separator)
	Sid = PMI__Button__DeconvolutionAnalysisRoi(id, value='Deconvolution (ROI)')
	Sid = PMI__Button__KidneyModelsROI(id, value = 'Kidney models (ROI)', /separator)
	Sid = PMI__Button__FitSingleInletRoi(id, value='Exchange models (ROI)')


end
