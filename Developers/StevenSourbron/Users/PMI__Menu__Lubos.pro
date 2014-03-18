pro PMI__Menu__Lubos, parent

	PMI__Menu__Skeleton, parent

	id = widget_button(parent, value='Lubos',/menu)

	Sid = PMI__Button__SemiQuantitativePerfusion(id, value='Semi-quantitative (Pixel)')
	Sid = PMI__Button__SemiQuantitativePerfusionRoi(id, value='Semi-quantitative (ROI)')

	Sid = PMI__Button__FastDeconvolutionAnalysis(id, value='Model-free (Pixel)', /separator)
	Sid = PMI__Button__DeconvolutionAnalysisRoi(id, value='Model-free (ROI)')

	Sid = PMI__Button__FitSingleInletRoi(id, value='Exchange models (ROI)', /separator)
	Sid = PMI__Button__KidneyModelsROI(id, value='Filtration models (ROI)')

	Sid = widget_button(id, value='Tools',/menu, /separator)

	SSid = PMI__Button__ExtractTimeWindow(Sid, value='Extract ROI window' )

end
