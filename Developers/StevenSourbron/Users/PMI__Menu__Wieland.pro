pro PMI__Menu__Wieland, parent

	PMI__Menu__Study, parent
	PMI__Menu__Series, parent
	PMI__Menu__Region, parent
	PMI__Menu__Window, parent
	PMI__Menu__Slices, parent
	PMI__Menu__Dynamic, parent
	PMI__Menu__Perfusion, parent

	id = widget_button(parent, value='Wieland',/menu)

	Sid = PMI__Button__SemiQuantitativePerfusion(id, value='Semi-quantitative analysis (Pixel)')
	Sid = PMI__Button__FastDeconvolutionAnalysis(id, value='Model-free analysis (Pixel)',/separator)
	Sid = PMI__Button__FastDeconvolutionAnalysisTrig(id, value='Model-free analysis (Pixel - Triggered)')
	Sid = PMI__Button__FitDualInletRoi(id, value='Fit dual-inlet model (ROI)',/separator)
	Sid = PMI__Button__FitDualInletRoiTrig(id, value='Fit dual-inlet model (ROI - Triggered)')

	Sid = PMI__Button__RegionSelectByRangeInteractive(id,value='Select ROI by range',/separator)

	Sid = widget_button(id, value='3D Reconstruction',/menu,/separator)

 	SSid = PMI__Button__SlicesAxialToCoronal(Sid,value='Axial -> Coronal')
 	SSid = PMI__Button__SlicesCoronalToSagittal(Sid,value='Coronal -> Sagittal')
 	SSid = PMI__Button__SlicesSagittalToAxial(Sid,value='Sagittal -> Axial')

	Sid = widget_button(id, value='Image processing',/menu)

 	SSid = PMI__Button__SlicesResolutionAnisotropic(Sid, value='Interpolate')
	SSid = PMI__Button__SlicesSmooth(Sid, value='Smooth')
	SSid = PMI__Button__SlicesMedianFilter(Sid, value='Median filter')


end
