


pro PMI__Menu__TRISTAN, parent

	PMI__Menu__Skeleton, parent
	PMI__Menu__Slices, parent
	PMI__Menu__Dynamic, parent
	PMI__Menu__Perfusion, parent


	id = widget_button(parent, value='MoCoMo',/menu)

    Sid = PMI__Button__MOCOMO_3D_DCE_2CM(id, value='3D DCE 2CM')
    Sid = PMI__Button__MOCOMO_3D_CONST(id, value='3D Constant')
    Sid = PMI__Button__MOCOMO_3D_VFA(id, value='3D VFA')
    Sid = PMI__Button__MOCOMO_2D_DCE_2CM(id, value='2D DCE 2CM', /separator)
    Sid = PMI__Button__MOCOMO_2D_DCE_1CM(id, value='2D DCE 1CM')
    Sid = PMI__Button__MOCOMO_2D_DCE_1CMD(id, value='2D DCE 1CM + Delay')
    Sid = PMI__Button__MOCOMO_2D_CONST(id, value='2D Constant')

    id = widget_button(parent, value='TRISTAN',/menu)
	Sid = PMI__Button__DumpDicom(id)

	Sid = PMI__Button__TRISTAN_Import(id, value = 'Import DICOMs for a single study')
	Sid = PMI__Button__TRISTAN_Batch_Import(id, value = 'Import DICOMs for several studies')
	Sid = PMI__Button__SeriesImportDicomSpecial(id, value = 'Import DICOM Special')

	Sid = PMI__Button__TRISTAN_LinearVFA_T1mapping(id, value='Variable Flip Angle T1-mapping (Pixel)',/separator)
	Sid = PMI__Button__TRISTAN_MOLLI_T1mapping(id, value='MOLLI T1-mapping (Pixel)')
	Sid = PMI__Button__FitT1SaturationRecoveryPixel(id, value='Saturation Recovery T1-mapping (Pixel)')

	Sid = PMI__Button__TRISTAN_CopyCoronalROIs(id, value='Copy ROI to BH and FB datasets',/separator)
	Sid = PMI__Button__TRISTAN_CopyTransverseROIs(id, value='Copy ROI to RAVE and MOLLI datasets')

	Sid = PMI__Button__SlicesCoronalToSagittal(id, value='CoronaltoSagittal',/separator)
	Sid = PMI__Button__SlicesSagittalToAxial(id, value='SagittaltoAxial',/separator)

	Sid = PMI__Button__TRISTAN_SLICESRESOLUTION(id, value='Change matrix size (isotropic)',/separator)
end
