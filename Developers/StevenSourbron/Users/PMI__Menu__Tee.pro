pro PMI__Menu__Tee, parent

	PMI__Menu__Skeleton, parent
	PMI__Menu__Slices, parent
	PMI__Menu__Dynamic, parent

	id = widget_button(parent, value='Tee',/menu)

	Sid = PMI__Button__SemiQuantitativePerfusion(id	, value='Semi-quantitative (Pixel)')
	Sid = PMI__Button__FastDeconvolutionAnalysis(id, value='Deconvolution (Pixel)')
	Sid = PMI__Button__KidneyModelsROI(id, value = 'Kidney models (ROI)', /separator)
;	Sid = PMI__Button__PatlakAnalysis(id, value='Patlak analysis (ROI)')

end
