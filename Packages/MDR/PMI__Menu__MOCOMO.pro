
pro PMI__Menu__MoCoMo, parent

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

    id = widget_button(parent, value='MoCoMo',/menu)

    Sid = PMI__Button__MOCOMO_3D_DCE_2CM(id, value='3D DCE 2CM')
    Sid = PMI__Button__MOCOMO_3D_CONST(id, value='3D Constant')
 ;   Sid = PMI__Button__MOCOMO_3D_VFA(id, value='3D VFA')
 	Sid = PMI__Button__MOCOMO_2D_DCE_2CM(id, value='2D DCE 2CM', /separator)
 	Sid = PMI__Button__MOCOMO_2D_CONST(id, value='2D Constant')
 ;   Sid = PMI__Button__MOCOMO_2D_DCE_1CM(id, value='2D DCE 1CM')
 ;   Sid = PMI__Button__MOCOMO_2D_DCE_1CMD(id, value='2D DCE 1CM + Delay')
 ;   Sid = PMI__Button__MOCOMO_2D_DCEmodelfree(id, value='2D DCE Model-Free')
 ;   Sid = PMI__Button__MOCOMO_2D_DTI(id, value='2D DTI', /separator)

end
