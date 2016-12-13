pro PMI__Menu__Katja, parent

	PMI__Menu__Skeleton, parent

	id = widget_button(parent, value='Katja',/menu)

	Sid = PMI__Button__SlicesSmooth(id, value='Images: Smooth')
	Sid = PMI__Button__SlicesMedianFilter(id, value='Images: Median filter')
 	Sid = PMI__Button__SlicesResolution(id, value='Images: Interpolate')

	Sid = PMI__Button__DynamicSmooth(id, value='Pixel curves: Smooth',/separator)
	Sid = PMI__Button__DynamicFastCollapse(id, value='Pixel curves: Parameters')
	Sid = PMI__Button__DynamicEnhancement(id, value='Pixel curves: Enhancement')

	Sid = PMI__Button__RegionSelectByRangeInteractive(id,value='Select ROI by value',/separator)

	Sid = PMI__Button__SemiQuantitativePerfusion(id, value='Perfusion: Semi-quantitative (Pixel)',/separator)
	Sid = PMI__Button__FastDeconvolutionAnalysis(id, value='Perfusion: Deconvolution (Pixel)')
	Sid = PMI__Button__SemiQuantitativePerfusionRoi(id	, value='Perfusion: Semi-quantitative (ROI)',/separator)
	Sid = PMI__Button__DeconvolutionAnalysisRoi(id		, value='Perfusion: Deconvolution (ROI)')
	Sid = PMI__Button__MM__Fit(id, value='Perfusion: Multi-model fit', /separator)
end
