pro PMI__Menu__Brain, parent

	PMI__Menu__Skeleton, parent
	PMI__Menu__Slices, parent
	PMI__Menu__Dynamic, parent

	id = widget_button(parent, value='Brain',/menu)

	Sid = PMI__Button__SemiQuantitativePerfusion(id	, value='Semi-quantitative (Pixel)')
	Sid = PMI__Button__SemiQuantitativePerfusionRoi(id, value='Semi-quantitative (ROI)')

	Sid = PMI__Button__FastDeconvolutionAnalysisPVcorr(id, value='Model-free (Pixel)',/separator)
	Sid = PMI__Button__DeconvolutionAnalysisRoiPVcorr(id, value='Model-free (ROI)')

	Sid = PMI__Button__FitSingleInletPatlakPVcorr(id, value='Model-based (Pixel)',/separator)
	Sid = PMI__Button__FitSingleInletRoiPVcorr(id, value='Model-based (ROI)')

	Sid = PMI__Button__FitT1SaturationRecoveryPixel(id, value='T1 mapping (SR)',/separator)
	Sid = PMI__Button__2d_Saturation_Recovery_Fit(id)
end
