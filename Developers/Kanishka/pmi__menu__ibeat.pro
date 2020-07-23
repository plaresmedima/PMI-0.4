;Menu for the demo version on
;https://sites.google.com/site/plaresmedima/

;REQUIRES PACKAGES:
;  Slices
;  Dynamic
;  Perfusion


pro PMI__Menu__iBeat, parent

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

;    id = widget_button(parent, value='MoCoMo',/menu)
;
;    Sid = PMI__Button__MOCOMO_3D_DCE_2CM(id, value='3D DCE 2CM')
;    Sid = PMI__Button__MOCOMO_3D_CONST(id, value='3D Constant')
;    Sid = PMI__Button__MOCOMO_2D_DCE_2CM(id, value='2D DCE 2CM', /separator)
;    Sid = PMI__Button__MOCOMO_2D_DCE_1CM(id, value='2D DCE 1CM')
;    Sid = PMI__Button__MOCOMO_2D_DCE_1CMD(id, value='2D DCE 1CM + Delay')
;    Sid = PMI__Button__MOCOMO_2D_CONST(id, value='2D Constant')

    id = widget_button(parent, value='iBEAt',/menu)

    Sid = PMI__Button__iBEAt_Import(id, value = 'Import DICOM')
    Sid = PMI__Button__MOCOMO_2D_DTI(id, value='DTI analysis')
    Sid = PMI__Button__MOCOMO_2D_DCE_2CM(id, value='DCE analysis')
 ;  Sid = PMI__Button__MOCOMO_2D_IVIM(id, value='IVIM analysis')

    Sid = PMI__Button__iBEAt_BatchProcessSingle(id, value = 'Batch process (single case)', /separator)
    Sid = PMI__Button__iBEAt_BatchProcessMultiple(id, value = 'Batch process (multiple cases)')

	id = widget_button(parent, value='Bashair',/menu)

	Sid = PMI__Button__SemiQuantitativePerfusion(id	, value='DCE semi-quantitative analysis')
	Sid = PMI__Button__FastDeconvolutionAnalysis(id, value='DCE model-free analysis')
	Sid = PMI__Button__MOCOMO_2D_DCE_2CM(id, value='DCE motion correction')
	Sid = PMI__Button__KidneyModelsROI_iBEAt(id, value = 'DCE ROI modelling')
	Sid = PMI__Button__DumpDicom(id, /separator)


;	Sid = PMI__Button__iBeat_Siemens_T1mapping(id, value = 'T1-mapping')

end
