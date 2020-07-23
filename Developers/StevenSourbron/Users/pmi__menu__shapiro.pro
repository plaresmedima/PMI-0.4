
pro PMI__Menu__Shapiro, parent

	PMI__Menu__Skeleton, parent
	PMI__Menu__Slices, parent
	PMI__Menu__Dynamic, parent

	id = widget_button(parent, value='Shapiro',/menu)

	Sid = PMI__Button__SemiQuantitativePerfusion(id	, value='Semi-quantitative (Pixel)')
	Sid = PMI__Button__FastDeconvolutionAnalysis(id, value='Model-free (Pixel)')
    Sid = PMI__Button__FitModToftsLin(id, value='Modified Tofts (Pixel)')

    Sid = PMI__Button__FitSingleInletRoi(id, value='Exchange models (ROI)', /separator)
	Sid = PMI__Button__KidneyModelsROI(id, value = 'Kidney models (ROI)')
	Sid = PMI__Button__FitDualInletRoi(id, value = 'Liver models (ROI)')

	Sid = PMI__Button__TristanRatsRoi_v2_1(id, value='Fit Rat Gadoxetate model v2.0 (ROI)', /separator)
	Sid = PMI__Button__HEPARIM_VFA(id, value='Variable Flip Angle T1-mapping', /separator)


end
