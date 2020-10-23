


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

    Sid = PMI__Button__iBEAt_Import(id, value = 'DICOM import')

    Sid = PMI__Button__iBEAt_T1mapMOLLI(id, value='T1 mapping', /separator)
    Sid = PMI__Button__iBEAt_T2star(id, value='T2* mapping')
    Sid = PMI__Button__iBEAt_T2Map(id, value='T2 mapping')
    Sid = PMI__Button__iBEAt_DTI(id, value='DTI mapping')
    Sid = PMI__Button__iBEAt_IVIM(id, value='IVIM mapping')
    Sid = PMI__Button__iBEAt_DCE(id, value='DCE mapping')
  ;  Sid = PMI__Button__iBEAt_MTR(id, value='MTR mapping')
    Sid = PMI__Button__iBEAt_Dummy(id, value='ASL mapping')

    Sid = PMI__Button__iBEAt_T1_ROI(id, value='T1 ROI analysis', /separator)
    Sid = PMI__Button__iBEAt_T2star_ROI(id, value='T2* ROI analysis')
    Sid = PMI__Button__iBEAt_T2_ROI(id, value='T2 ROI analysis')
    Sid = PMI__Button__iBEAt_Dummy(id, value='DTI ROI analysis')
    Sid = PMI__Button__iBEAt_Dummy(id, value='IVIM ROI analysis')
    Sid = PMI__Button__iBEAt_DCE_ROI(id, value='DCE ROI analysis')
    Sid = PMI__Button__iBEAt_Dummy(id, value='MTR ROI analysis')
    Sid = PMI__Button__iBEAt_Dummy(id, value='ASL ROI analysis')
    Sid = PMI__Button__iBEAt_Dummy(id, value='PC ROI analysis')

    Sid = PMI__Button__iBEAt_Dummy(id, value='DCE AIF detection', /separator)
    Sid = PMI__Button__iBEAt_Dummy(id, value='PC ROI detection')
    Sid = PMI__Button__iBEAt_Dummy(id, value='Sequence co-registration')
    Sid = PMI__Button__iBEAt_Dummy(id, value='Kidney ROI detection')
    Sid = PMI__Button__iBEAt_Dummy(id, value='Cortex ROI detection')

    Sid = PMI__Button__iBEAt_Dummy(id, value = 'Batch processing (single case)', /separator)
    Sid = PMI__Button__iBEAt_Dummy(id, value = 'Batch processing (multiple cases)')

	id = widget_button(parent, value='Bashair',/menu)

	Sid = PMI__Button__SemiQuantitativePerfusion(id	, value='DCE semi-quantitative analysis')
	Sid = PMI__Button__FastDeconvolutionAnalysis(id, value='DCE model-free analysis')
	Sid = PMI__Button__MOCOMO_2D_DCE_2CM(id, value='DCE motion correction')
	Sid = PMI__Button__KidneyModelsROI_iBEAt(id, value = 'DCE ROI modelling')
	Sid = PMI__Button__DumpDicom(id, /separator)


end
