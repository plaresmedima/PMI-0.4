pro PMI__Menu__Anja, parent

	PMI__Menu__Skeleton, parent

	id = widget_button(parent, value='Anja',/menu)

	Sid = PMI__Button__AnjaSemiQuantitative(id,value='Area Under the Curve (Pixel)')
	Sid = PMI__Button__AnjaDeconvolution(id,value='Quantitative analysis (Pixel)',/separator)
	Sid = PMI__Button__AnjaRoiModels(id,value='Quantitative analysis (ROI)')

end
