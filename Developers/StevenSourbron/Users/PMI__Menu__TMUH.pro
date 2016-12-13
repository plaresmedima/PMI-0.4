pro PMI__Menu__TMUH, parent

	PMI__Menu__Skeleton, parent
	PMI__Menu__Slices, parent
	PMI__Menu__Dynamic, parent
	PMI__Menu__Perfusion, parent

	id = widget_button(parent, value='TMUH (Hai Gan)',/menu)

	Sid = PMI__Button__TMUH_SeriesImportDicom(id, value='Import DICOM')
	Sid = PMI__Button__TMUH_SeriesMerge(id,value='Merge VIBE')
	Sid = PMI__Button__TMUH_FitOneCompartment(id, value='ROI analysis',/separator)
	Sid = PMI__Button__TMUH_FitOneCompartmentPixel(id, value='Pixel analysis',/separator)

	id = widget_button(parent, value='TMUH (Liver DCE)',/menu)

	Sid = PMI__Button__TMUH_SeriesImportDicom(id, value='Import DICOM')
	Sid = PMI__Button__TMUH_LiverDCE_SeriesMergeAxial(id,value='Merge 3D data (axial)',/separator)
	Sid = PMI__Button__TMUH_LiverDCE_SeriesMerge(id,value='Merge 3D data (coronal)')
	Sid = PMI__Button__TMUH_FitPatlak(id, value='ROI analysis (liver)',/separator)
	Sid = PMI__Button__TMUH_FitOneCompartment(id, value='ROI analysis (tumor)')
	Sid = PMI__Button__TMUH_FitPatlakPixel(id, value='Pixel analysis (liver)',/separator)
	Sid = PMI__Button__TMUH_FitOneCompartmentPixel(id, value='Pixel analysis (tumor)')

	id = widget_button(parent, value='TMUH (Liver DSC)',/menu)

	Sid = PMI__Button__TMUH_SeriesImportDicom(id, value='Import DICOM')
	Sid = PMI__Button__TMUH_USPIOSemiQuantitative(id	, value='Pixel analysis (semi-quantitative)', /separator)
	Sid = PMI__Button__TMUH_USPIOFastDeconvolutionAnalysis(id, value='Pixel analysis')
	Sid = PMI__Button__TMUH_USPIO_DeconvolutionAnalysisRoi(id, value='ROI analysis', /separator)

	id = widget_button(parent, value='TMUH (Liver CT)',/menu)

	Sid = PMI__Button__TMUH_SeriesImportDicom(id, value='Import DICOM')
	Sid = PMI__Button__TMUH_CTSemiQuantitative(id	, value='Pixel analysis (semi-quantitative)', /separator)
	Sid = PMI__Button__TMUH_CTFastDeconvolutionAnalysis(id, value='Pixel analysis')
	Sid = PMI__Button__TMUH_CT_FitExchangeRoi(id, value='ROI analysis (tumor)', /separator)

	id = widget_button(parent, value='TMUH (Tools)',/menu)

	Sid = PMI__Button__TMUH_RegionExtrude(id, value = 'Extrude Region')
	Sid = PMI__Button__RegionSelectByRangeInteractive(id, value='Threshold Region')

end
