;Menu for the demo version on
;https://sites.google.com/site/plaresmedima/



pro PMI__Menu__Sugar, parent

	PMI__Menu__Skeleton, parent
	PMI__Menu__Slices, parent
	PMI__Menu__Dynamic, parent

	id = widget_button(parent, value='Perfusion',/menu)

	Sid = PMI__Button__SemiQuantitativePerfusion(id	, value='Semi-quantitative (Pixel)')
	Sid = PMI__Button__FastDeconvolutionAnalysis(id, value='Model-free (Pixel)')
    Sid = PMI__Button__FitModToftsLin(id, value='Modified Tofts (Pixel)')
    Sid = PMI__Button__FitSingleInletRoi(id, value='Exchange models (ROI)', /separator)
	Sid = PMI__Button__KidneyModelsROI(id, value = 'Kidney models (ROI)')
	Sid = PMI__Button__FitDualInletRoi(id, value = 'Liver models (ROI)')

    id = widget_button(parent, value='SUGAR',/menu)

	Sid = PMI__Button__SemiQuantitativePerfusion(id	, value='Renal: Semi-quantitative (Pixel)')
	Sid = PMI__Button__MOCOMO_2D_DCE_2CM(id			, value='Renal: Motion correction')
	Sid = PMI__Button__FastDeconvolutionAnalysis(id	, value='Renal: Model-free (Pixel)')
	Sid = PMI__Button__KidneyModelsROI_iBEAt(id		, value='Renal: Kidney models (ROI)')

	Sid = PMI__Button__SemiQuantitativePerfusion(id	, value='Cardiac: Descriptive parameter maps', /separator)
	Sid = PMI__Button__SohaibCompartmentLinear(id	, value='Cardiac: Perfusion parameter maps')
    Sid = PMI__Button__FitSingleInletRoi(id			, value='Cardiac: Perfusion ROI compartment models')


end
