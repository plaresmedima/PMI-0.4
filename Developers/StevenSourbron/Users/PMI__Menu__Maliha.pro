pro PMI__Menu__Maliha, parent

	PMI__Menu__Skeleton, parent

	id = widget_button(parent, value='Maliha',/menu)

	Sid = PMI__Button__MalihaSemiQuantitative(id)
	Sid = PMI__Button__MalihaDeconvolution(id)
	Sid = PMI__Button__MalihaSelectRegionByRange(id)
	Sid = PMI__Button__MalihaROIfit(id)
end
