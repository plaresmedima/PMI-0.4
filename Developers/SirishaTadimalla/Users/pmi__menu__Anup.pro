
pro PMI__Menu__Anup, parent

	PMI__Menu__Skeleton, parent
	PMI__Menu__Slices, parent
	PMI__Menu__Dynamic, parent

    id = widget_button(parent, value='Perfusion',/menu)

	Sid = PMI__Button__anup_fitting(id	, value='Anup fitting (ROI)', /separator)

end
