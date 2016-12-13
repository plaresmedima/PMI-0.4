pro PMI__Menu__Daniel, parent

	PMI__Menu__Study, parent
	PMI__Menu__Series, parent
	PMI__Menu__Region, parent
	PMI__Menu__Window, parent

	id = widget_button(parent, value='Daniel',/menu)

	Sid = PMI__Button__SemiQuantitativePerfusion(id	, value='Semi-quantitative maps')
	Sid = PMI__Button__FastDeconvolutionAnalysis(id	, value='Quantitative maps')
	Sid = PMI__Button__FitSingleInletRoi(id, value='All exchange models (ROI)')

	Sid = PMI__Button__DynamicEnhancement(id, value = 'Enhancement' , /separator)
	Sid = PMI__Button__DynamicFastCollapse(id, value = 'Dynamic parameters')
	Sid = PMI__Button__SlicesCollapse(id, value = 'Volume projections')



end
