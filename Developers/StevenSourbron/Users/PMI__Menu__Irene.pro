pro PMI__Menu__Irene, parent

	PMI__Menu__Skeleton, parent
	PMI__Menu__Slices, parent
	PMI__Menu__Dynamic, parent

	id = widget_button(parent, value='Irene',/menu)

	Sid = PMI__Button__SemiQuantitativePerfusion(id	, value='Semi-quantitative maps')
	Sid = PMI__Button__FitSingleInletRoiIrene(id, value='ROI-based analysis')

	Sid = PMI__Button__FitSingleInletPatlakIrene(id, value='Pixel-based analysis')
	Sid = PMI__Button__FitT1SaturationRecoveryPixel(id, value='T1 mapping')

end
