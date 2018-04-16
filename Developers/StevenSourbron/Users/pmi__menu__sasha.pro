;Menu for the demo version on
;https://sites.google.com/site/plaresmedima/

;REQUIRES PACKAGES:
;  Slices
;  Dynamic
;  Perfusion

;
;    Copyright (C) 2018 Steven Sourbron
;



pro PMI__Menu__Sasha, parent

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

    id = widget_button(parent, value='Sasha',/menu)

	Sid = PMI__Button__SemiQuantitativePerfusion(id	, value='Semi-quantitative (Pixel)')
    Sid = PMI__Button__MOCO_2D_DCE_2CM(id, value='MoCo Kidneys',/separator)
    Sid = PMI__Button__MOCO_2D_DCE_1CMD(id, value='MoCo Heart (WIP)')
    Sid = PMI__Button__FastDeconvolutionAnalysis(id, value='Model-free (Pixel)',/separator)
    Sid = PMI__Button__SohaibCompartmentLinear(id,value='One compartment (Pixel)')
    Sid = PMI__Button__SohaibFermiDeconvolution(id,value='Fermi model (Pixel)')
    Sid = PMI__Button__KidneyModelsROI(id, value = 'Kidney models (ROI)', /separator)
    Sid = PMI__Button__CardiacSingleInletRoiSlider(id, value='Cardiac Models (ROI)')


end
