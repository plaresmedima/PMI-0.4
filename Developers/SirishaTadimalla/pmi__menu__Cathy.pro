
pro PMI__Menu__Cathy, parent

	PMI__Menu__Skeleton, parent
	PMI__Menu__Slices, parent
	PMI__Menu__Dynamic, parent

    id = widget_button(parent, value='TRISTAN Rat MSD',/menu)

;	Sid = PMI__Button__SeriesImportTristanRatDicom(id, value='Import DICOMs')
	Sid = PMI__Button__TristanSemiQuantitativePerfusion(id	, value='Create time-MIPs')
	Sid = PMI__Button__TristanPrepareVFA(id, value='Prepare VFA data for fitting')
	Sid = PMI__Button__TRISTAN_LinearVFA_T1mapping(id, value='Variable Flip Angle T1-mapping (Pixel)')
;	Sid = PMI__Button__tristanratsroi(id, value='Fit Rat Gadoxetate model v1.0')
;	Sid = PMI__Button__tristanratsroi_v1_1(id, value='Fit Rat Gadoxetate model v1.1')
	Sid = PMI__Button__tristanratsroi_v1_2(id, value='Fit Rat Gadoxetate model v1.2')

end
