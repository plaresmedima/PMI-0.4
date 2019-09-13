

pro PMI__Menu__TRISTANRATS, parent

	PMI__Menu__Skeleton, parent
	PMI__Menu__Slices, parent
	PMI__Menu__Dynamic, parent

    id = widget_button(parent, value='TRISTAN Rat Assay',/menu)

	Sid = PMI__Button__SeriesImportTristanRatDicom(id, value='Import DICOM v2.0 ')
	Sid = PMI__Button__TristanRatsRoi_v2_0(id, value='Fit Rat Gadoxetate model v2.0')
	Sid = PMI__Button__TristanRatVFA(id, value='Variable Flip Angle T1-mapping', /separator)

;	Sid = PMI__Button__tristanratsroi(id, value='Fit Rat Gadoxetate model v1.0')
;	Sid = PMI__Button__TristanSemiQuantitativePerfusion(id	, value='Create time-MIPs')

;	Sid = PMI__Button__tristanratsroi_v1_1(id, value='Fit Rat Gadoxetate model v1.1')
;	Sid = PMI__Button__tristanratsroi_v1_2(id, value='Fit Rat Gadoxetate model v1.2')


end
