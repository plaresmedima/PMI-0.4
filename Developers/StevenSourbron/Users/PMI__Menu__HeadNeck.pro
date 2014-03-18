pro PMI__Menu__HeadNeck, parent

	PMI__Menu__Skeleton, parent
	PMI__Menu__Slices, parent
	PMI__Menu__Dynamic, parent

	id = widget_button(parent, value='H+N',/menu)

	Sid = PMI__Button__SemiQuantitativePerfusion(id	, value='Semi-quantitative (Pixel)')
	Sid = PMI__Button__SemiQuantitativePerfusionRoi(id, value='Semi-quantitative (ROI)')

	Sid = PMI__Button__FastDeconvolutionAnalysisPVcorr(id, value='Model-free (Pixel)',/separator)
	Sid = PMI__Button__DeconvolutionAnalysisRoiPVcorr(id, value='Model-free (ROI)')

	Sid = PMI__Button__FitSingleInletPatlakPVcorr(id, value='Model-based (Pixel)',/separator)
	Sid = PMI__Button__FitSingleInletRoiPVcorr(id, value='Model-based (ROI)')

	Sid = PMI__Button__RegionImport_RT(id, value='Import radiotherapy contour',/separator)
;	Sid = PMI__Button__DumpDicom(id)

end
