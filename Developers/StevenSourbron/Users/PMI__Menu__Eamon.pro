pro PMI__Menu__Eamon, parent

	PMI__Menu__Study, parent
	PMI__Menu__Series, parent
	PMI__Menu__Region, parent
	PMI__Menu__Window, parent
	PMI__Menu__Slices, parent
	PMI__Menu__Dynamic, parent

	id = widget_button(parent, value='Eamon',/menu)


	Sid = PMI__Button__SemiQuantitativePerfusion(id	, value='Semi-quantitative perfusion (Pixel)')
	Sid = PMI__Button__FastDeconvolutionAnalysis(id, value='Model-free perfusion (Pixel)')
	Sid = PMI__Button__DeconvolutionAnalysisRoi(id, value='Model-free perfusion (ROI)', /separator)
	Sid = PMI__Button__FitSingleInletRoi(id, value='Model-based perfusion (ROI)')

	Sid = PMI__Button__RegionSelectByRangeInteractive(id, value='Select ROI by range',/separator)


end
