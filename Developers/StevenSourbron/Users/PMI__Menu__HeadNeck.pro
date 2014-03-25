pro PMI__Menu__HeadNeck, parent

	PMI__Menu__Skeleton, parent
	PMI__Menu__Slices, parent
	PMI__Menu__Dynamic, parent

	id = widget_button(parent, value='Perfusion',/menu)

	Sid = PMI__Button__SemiQuantitativePerfusion(id	, value='Semi-quantitative (Pixel)')
	Sid = PMI__Button__FastDeconvolutionAnalysis(id, value='Model-free (Pixel)')
    Sid = PMI__Button__FitModToftsLin(id, value='Modified Tofts (Pixel)')

    Sid = PMI__Button__FitSingleInletRoi(id, value='Exchange models (ROI)', /separator)
	Sid = PMI__Button__KidneyModelsROI(id, value = 'Kidney models (ROI)')
	Sid = PMI__Button__FitDualInletRoi(id, value = 'Liver models (ROI)')

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
