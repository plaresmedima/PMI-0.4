pro PMI__Menu__Thomas, parent

	PMI__Menu__Study, parent
	PMI__Menu__Series, parent
	PMI__Menu__Region, parent
	PMI__Menu__Window, parent

	id = widget_button(parent, value='Thomas',/menu)

	Sid = PMI__Button__SemiQuantitativePerfusion(id	, value='Semi-quantitative DCE (Pixel)')
	Sid = PMI__Button__SemiQuantitativePerfusionRoi(id, value='Semi-quantitative DCE (ROI)')

	Sid = PMI__Button__RegionSelectByRangeInteractive(id, value='Interactive ROI selection (threshold)', /separator)
	Sid = PMI__Button__DynamicEnhancement(id, value='Dynamic Signal Enhancement (pixel)')

end
