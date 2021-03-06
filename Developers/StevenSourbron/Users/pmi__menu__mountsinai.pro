
pro PMI__Menu__MountSinai, parent

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

    id = widget_button(parent, value='Mount Sinai',/menu)

	Sid = PMI__Button__Sergios_FitDualInletRoi(id, value = 'Liver DCE (ROI)')
	Sid = PMI__Button__Sergios_DualInletUptakePixel(id, value='Liver DCE (Pixel)')
	Sid = PMI__Button__Sergios_DualInletUptakePixel_Orig(id, value='Liver DCE with T1-map (Pixel)')
	Sid = PMI__Button__HEPARIM_VFA(id, value='Variable Flip Angle T1-mapping')

	Sid = widget_button(id, value='Model-driven registration',/menu)

	SSid = PMI__Button__MOCOMO_2D_CONST(Sid, value='2D Constant')
 	SSid = PMI__Button__MOCOMO_2D_DCE_2CM(Sid, value='2D DCE 2CM')
    SSid = PMI__Button__MOCOMO_2D_DCE_1CM(Sid, value='2D DCE 1CM')

	SSid = PMI__Button__MOCOMO_3D_CONST(Sid, value='3D Constant', /separator)
	SSid = PMI__Button__MOCOMO_3D_DCE_2CM(Sid, value='3D DCE 2CM')
    SSid = PMI__Button__MOCOMO_3D_VFA(Sid, value='3D VFA')

	Sid = widget_button(id, value='Reslice 3D',/menu)

	SSid = PMI__Button__SlicesAxialToCoronal(Sid, value='Axial to coronal')
	SSid = PMI__Button__SlicesCoronalToSagittal(Sid, value='Coronal to sagittal')
	SSid = PMI__Button__SlicesSagittalToAxial(Sid, value='Sagittal to axial')

	Sid = PMI__Button__DumpDicom(id)


end
