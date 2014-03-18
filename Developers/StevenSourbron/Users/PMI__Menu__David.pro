pro PMI__Menu__David, parent

	PMI__Menu__Study, parent
	PMI__Menu__Series, parent
	PMI__Menu__Region, parent
	PMI__Menu__Window, parent
	PMI__Menu__Slices, parent
	PMI__Menu__Dynamic, parent
	PMI__Menu__Perfusion, parent

	id = widget_button(parent, value='David',/menu)

	Sid = PMI__Button__SeriesExportCsv(id,/separator)
	Sid = PMI__Button__SeriesExportChecker(id)
	Sid = PMI__Button__FusionExportColorMix(id)
	Sid = PMI__Button__SlicesGradient(id)
	Sid = PMI__Button__KidneyModelsROI(id,/separator)
	Sid = PMI__Button__ToftsKidneyModelRoi(id)
	Sid = PMI__Button__FitPatlakLinearRoi(id, value='Fit Linearized Patlak (ROI)')
	Sid = PMI__Button__ImportParRec(id)
	Sid = PMI__Button__DumpDicom(id)

	id = widget_button(parent, value='Arshad',/menu)

	Sid = PMI__Button__SlicesResolution(id,value='Interpolate images')
	Sid = PMI__Button__ExtractTimeWindow(id,value='Remove motion')
	Sid = PMI__Button__ArshadDeconvolution(id,value='Quantitative analysis (Pixel)',/separator)
	Sid = PMI__Button__ArshadDeconvolutionRoi(id,value='Quantitative analysis (ROI)')
	Sid = PMI__Button__ArshadFermiRoi(id,value='Fermi model analysis (ROI)')
	Sid = PMI__Button__ArshadFermiRoiStressRest(id,value='T1-corrected Fermi model analysis (ROI)')
	Sid = PMI__Button__SlicesSmooth(id, value='Uniform slice smoothing')
	Sid = PMI__Button__DynamicSmooth(id, value='Uniform curve smoothing')

	id = widget_button(parent, value='Chris',/menu)

	Sid = PMI__Button__SemiQuantitativePerfusion(id	, value='Semi-quantitative (Pixel)')
	Sid = PMI__Button__SemiQuantitativePerfusionRoi(id, value='Semi-quantitative (ROI)')
	Sid = PMI__Button__FastDeconvolutionAnalysis(id, value='Deconvolution (Pixel)', /separator)
	Sid = PMI__Button__DeconvolutionAnalysisRoi(id, value='Deconvolution (ROI)')
	Sid = PMI__Button__KidneyModelsROI(id, value = 'All kidney models (ROI)', /separator)
	Sid = PMI__Button__MaximumSlopeRoi(id, value='Maximum slope (ROI)')
	Sid = PMI__Button__FitPatlakLinearRoi(id, value='Fit Linearized Patlak (ROI)')

	id = widget_button(parent, value='Jemma',/menu)

	Sid = PMI__Button__SemiQuantitativePerfusion(id	, value='Semi-quantitative (Pixel)')
	Sid = PMI__Button__FastDeconvolutionAnalysis(id, value='Deconvolution (Pixel)')
	Sid = PMI__Button__FitSingleInletRoi(id, value='2-compartment models (ROI)')
	Sid = PMI__Button__DeconvolutionAnalysisRoi(id, value='Deconvolution (ROI)', /separator)
	Sid = PMI__Button__FitAATHRoi(id, value='AATH model (ROI)')
	Sid = PMI__Button__DumpDicom(id,/separator)

	id = widget_button(parent, value='Tze',/menu)

	Sid = PMI__Button__FastDeconvolutionAnalysis(id, value='Deconvolution (Pixel)', /separator)
	Sid = PMI__Button__MaximumSlopeRoi(id, value='Maximum slope (ROI)')
	Sid = PMI__Button__DeconvolutionAnalysisRoi(id, value='Deconvolution (ROI)')
	Sid = PMI__Button__FitToftsDelayRoi(id, value='One compartment (ROI)')

	id = widget_button(parent, value='Stephen',/menu)

	Sid = PMI__Button__SemiQuantitativePerfusion(id	, value='Semi-quantitative (Pixel)')
	Sid = PMI__Button__SemiQuantitativePerfusionRoi(id, value='Semi-quantitative (ROI)')
	Sid = PMI__Button__FastDeconvolutionAnalysis(id, value='Deconvolution (Pixel)', /separator)
	Sid = PMI__Button__DeconvolutionAnalysisRoi(id, value='Deconvolution (ROI)')
	Sid = PMI__Button__KidneyModelsROI(id, value = 'All kidney models (ROI)', /separator)
	Sid = PMI__Button__MaximumSlopeRoi(id, value='Maximum slope (ROI)')
	Sid = PMI__Button__FitPatlakLinearRoi(id, value='Fit Linearized Patlak (ROI)')
	Sid = PMI__Button__FitToftsDelayRoi(id, value='One compartment (ROI)')

end
