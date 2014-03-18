pro PMI__Menu__ahmad, parent

	PMI__Menu__Study, parent
	PMI__Menu__Series, parent
	PMI__Menu__Region, parent
	PMI__Menu__Window, parent
	PMI__Menu__Slices, parent

	id = widget_button(parent, value='Ahmad',/menu)

	Sid = PMI__Button__SemiQuantitativePerfusion(id, value='Semi-quantitative analysis (Pixel)')
	Sid = PMI__Button__FitDualInletRoi(id, value='All dual-inlet models (ROI)')


end
