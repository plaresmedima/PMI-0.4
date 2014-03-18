pro PMI__Menu__Greg, parent

	PMI__Menu__Skeleton, parent
	PMI__Menu__Slices, parent
	PMI__Menu__Dynamic, parent

	id = widget_button(parent, value='Greg',/menu)

	Sid = PMI__Button__SemiQuantitativePerfusion(id	, value='Semi-quantitative (Pixel)')
	Sid = PMI__Button__SemiQuantitativePerfusionRoi(id, value='Semi-quantitative (ROI)')

	Sid = PMI__Button__FastDeconvolutionAnalysis(id, value='Model-free (Pixel)',/separator)
	Sid = PMI__Button__DeconvolutionAnalysisRoi(id, value='Model-free (ROI)')

	Sid = PMI__Button__FitModToftsLin(id, value = 'Modified Tofts (Pixel)', /separator)
	Sid = PMI__Button__FitSingleInletRoiTze(id, value='Exchange models (ROI)')

	Sid = PMI__Button__FitSingleInletRoiPVcorr(id, value='Exchange models+ (ROI)', /separator)
end
