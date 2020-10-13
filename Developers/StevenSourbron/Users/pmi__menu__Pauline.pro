



pro PMI__Menu__Pauline, parent

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

    id = widget_button(parent, value='Pauline',/menu)

	Sid = PMI__Button__SemiQuantitativePerfusion(id	, value='Semi-quantitative (Pixel)')
	Sid = PMI__Button__FastDeconvolutionAnalysis(id, value='Model-free (Pixel)')
	Sid = PMI__Button__MOCOMO_2D_DCE_2CM(id, value='Motion correction')
	Sid = PMI__Button__KidneyModelsROI_iBEAt(id, value = 'Kidney models (ROI)')

;	Sid = PMI__Button__iBEAt_Import(id, value = 'Import DICOM')
;	Sid = PMI__Button__DumpDicom(id, /separator)

;	Sid = PMI__Button__iBeat_Siemens_T1mapping(id, value = 'T1-mapping')

end
