pro PMI__Menu__Jemma, parent

	PMI__Menu__Study, parent
	PMI__Menu__Series, parent
	PMI__Menu__Region, parent
	PMI__Menu__Window, parent
	PMI__Menu__Slices, parent
	PMI__Menu__Dynamic, parent

	id = widget_button(parent, value='Jemma',/menu)

	Sid = PMI__Button__SemiQuantitativePerfusion(id	, value='Semi-quantitative (Pixel)')
	Sid = PMI__Button__FastDeconvolutionAnalysis(id, value='Deconvolution (Pixel)')
	Sid = PMI__Button__FitSingleInletRoi(id, value='2-compartment models (ROI)')
	Sid = PMI__Button__DeconvolutionAnalysisRoi(id, value='Deconvolution (ROI)', /separator)
	Sid = PMI__Button__FitAATHRoi(id, value='AATH model (ROI)')
	Sid = PMI__Button__RegionSelectByRangeInteractive(id, value='Select ROI by Range')
	Sid = PMI__Button__DumpDicom(id,/separator)
end
