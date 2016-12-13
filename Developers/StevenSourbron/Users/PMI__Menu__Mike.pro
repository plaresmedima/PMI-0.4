pro PMI__Menu__Mike, parent

	PMI__Menu__Study, parent
	PMI__Menu__Series, parent
	PMI__Menu__Region, parent
	PMI__Menu__Window, parent
	PMI__Menu__Slices, parent
	PMI__Menu__Dynamic, parent
	PMI__Menu__Perfusion, parent
	PMI__Menu__Relaxation, parent

	id = widget_button(parent, value='Mike',/menu)

	Sid = PMI__Button__ImportParRec(id)

	Sid = PMI__Button__TriggeredFastDeconvolution(id,value='Pixel-by-pixel deconvolution (with triggering)',/separator)
	Sid = PMI__Button__FastDeconvolutionAnalysis(id, value='Pixel-by-pixel deconvolution (without triggering)')
	Sid = PMI__Button__FastDeconvolutionAnalysisStdAif(id, value='Pixel-by-pixel deconvolution (Standard AIF)')

	Sid = PMI__Button__TriggeredKidneyFiltrationRoi(id, value='ROI filtration model (with triggering)', /separator)
	Sid = PMI__Button__KidneyFiltrationRoi(id, value='ROI filtration model (without triggering)')
	Sid = PMI__Button__KidneyFiltrationRoiStdAif(id, value='ROI filtration model (Standard AIF)')
	Sid = PMI__Button__KidneyModelsROI(id,value='All ROI filtration models')

	Sid = PMI__Button__RegionSelectByRangeInteractive(id,value='Select ROI by range', /separator)


end
