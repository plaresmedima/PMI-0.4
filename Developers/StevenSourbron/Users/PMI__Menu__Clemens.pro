pro PMI__Menu__Clemens, parent

	PMI__Menu__Skeleton, parent

	id = widget_button(parent, value='Clemens',/menu)

	Sid = PMI__Button__FitPatlakModelClemens(id, value='Albumin model (ROI)')

	Sid = PMI__Button__SemiQuantitativePerfusion(id, value='Semi-quantitative (Pixel)', /separator)
	Sid = PMI__Button__FastDeconvolutionAnalysis(id, value='Quantitative (Pixel)')
	Sid = PMI__Button__FitExchangeDelayRoi(id, value='Gadovist Tumor model (ROI)', /separator)
	Sid = PMI__Button__FitToftsDelayRoi(id, value='Gadovist Muscle model (ROI)')
;	Sid = PMI__Button__GadovistModelROI(id, value='Gadovist model (ROI)')


end
