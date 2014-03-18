pro PMI__Menu__eli, parent

	PMI__Menu__Skeleton, parent
	PMI__Menu__Slices, parent
	PMI__Menu__Dynamic, parent

	id = widget_button(parent, value='Eli',/menu)

	Sid = PMI__Button__SemiQuantitativePerfusion(id	, value='Semi-quantitative (Pixel)')
	Sid = PMI__Button__KidneyModelsROI(id)
	Sid = PMI__Button__FastDeconvolutionAnalysis(id, value='Deconvolution (Fast)')



end
