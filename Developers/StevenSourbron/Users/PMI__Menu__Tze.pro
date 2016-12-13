pro PMI__Menu__Tze, parent

	PMI__Menu__Study, parent
	PMI__Menu__Series, parent
	PMI__Menu__Region, parent
	PMI__Menu__Window, parent
	PMI__Menu__Slices, parent

	id = widget_button(parent, value='Tze',/menu)

	Sid = PMI__Button__SemiQuantitativePerfusion(id	, value='Semi-quantitative (Pixel)')
	Sid = PMI__Button__FastDeconvolutionAnalysis(id, value='Deconvolution (Pixel)')
	Sid = PMI__Button__KidneyModelsROI(id, value = 'Kidney models (ROI)', /separator)
	Sid = PMI__Button__FitSingleInletRoiTze(id, value='Exchange models (ROI)')


end
