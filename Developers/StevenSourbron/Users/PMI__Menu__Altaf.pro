
pro PMI__Menu__Altaf, parent

	PMI__Menu__Skeleton, parent
	PMI__Menu__Slices, parent
	PMI__Menu__Dynamic, parent

	id = widget_button(parent, value='HEPARIM',/menu)

	Sid = PMI__Button__SemiQuantitativePerfusion(id	, value='Descriptive parameter mapping')
	Sid = PMI__Button__HEPARIM_PILOT_FitDualInletRoi(id, value = 'Liver ROI Analysis')


end
