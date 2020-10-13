


pro PMI__Menu__Mangion, parent

	PMI__Menu__Skeleton, parent
	PMI__Menu__Slices, parent
	PMI__Menu__Dynamic, parent

	id = widget_button(parent, value='Mangion',/menu)

	Sid = PMI__Button__SemiQuantitativePerfusion(id, value='Descriptive parameter maps')
	Sid = PMI__Button__SohaibCompartmentLinear(id,value='Perfusion parameter maps')
    Sid = PMI__Button__FitSingleInletRoi(id, value='ROI compartment models')


end
