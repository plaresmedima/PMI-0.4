pro PMI__Menu__Martine, parent

	PMI__Menu__Study, parent
	PMI__Menu__Series, parent
	PMI__Menu__Region, parent
	PMI__Menu__Window, parent
	PMI__Menu__Slices, parent
	PMI__Menu__Dynamic, parent

	id = widget_button(parent, value='Perfusion',/menu)

	Sid = PMI__Button__SemiQuantitativePerfusion(id	, value='Semi-quantitative (Pixel)')
	Sid = PMI__Button__FastDeconvolutionAnalysis(id, value='Deconvolution (Pixel)')

	Sid = PMI__Button__SemiQuantitativePerfusionRoi(id, value='Semi-quantitative (ROI)', /separator)
	Sid = PMI__Button__DeconvolutionAnalysisRoi(id, value='Deconvolution (ROI)')
	Sid = PMI__Button__FitSingleInletRoi(id, value='Model-based (ROI)')
end
