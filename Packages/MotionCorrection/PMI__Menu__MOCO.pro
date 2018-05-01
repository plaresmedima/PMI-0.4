;Menu for the demo version on
;https://sites.google.com/site/plaresmedima/

;REQUIRES PACKAGES:
;  Slices
;  Dynamic
;  Perfusion

;
;    Copyright (C) 2018 Steven Sourbron
;



pro PMI__Menu__MoCo, parent

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

    id = widget_button(parent, value='MoCo',/menu)

    Sid = PMI__Button__MOCO_3D_DCE_2CM(id, value='3D DCE 2CM')
    Sid = PMI__Button__MOCO_2D_DCE_2CM(id, value='2D DCE 2CM')
    Sid = PMI__Button__MOCO_2D_DCE_1CM(id, value='2D DCE 1CM')
    Sid = PMI__Button__MOCO_2D_DCE_1CMD(id, value='2D DCE 1CM + Delay')
    Sid = PMI__Button__MOCO_3D_DYN(id, value='3D Dynamic')



end
