pro PMI__Menu__Glaser, parent

	PMI__Menu__Study, parent
	PMI__Menu__Series, parent
	PMI__Menu__Region, parent
	PMI__Menu__Window, parent
	PMI__Menu__Slices, parent
	PMI__Menu__Dynamic, parent

	id = widget_button(parent, value='Glaser',/menu)

	Sid = PMI__Button__RegionSelectByRangeInteractive(id, value='Select ROI by threshold values')

	Sid = PMI__Button__vTE_T2_Pixel(id, value='T2 fit (Pixel)', /separator)
	Sid = PMI__Button__2d_vTE_T2_Fit(id, value='T2 fit (ROI)')
	Sid = PMI__Button__SemiQuantitativePerfusion(id	, value='Semi-quantitative perfusion (Pixel)', /separator)
	Sid = PMI__Button__SemiQuantitativePerfusionRoi(id, value='Semi-quantitative perfusion (ROI)')
	Sid = PMI__Button__FastDeconvolutionAnalysis(id, value='Quantitative perfusion (Pixel)', /separator)
	Sid = PMI__Button__FitSingleInletRoi(id	, value='Quantitative perfusion (ROI)')



end
