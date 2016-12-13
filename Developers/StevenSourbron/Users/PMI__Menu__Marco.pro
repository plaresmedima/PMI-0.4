pro PMI__Menu__Marco, parent

	PMI__Menu__Skeleton, parent
	PMI__Menu__Slices, parent
	PMI__Menu__Dynamic, parent

	PMI__Menu__PerfusionSteven, parent

	id = widget_button(parent, value='Brain',/menu)

	Sid = PMI__Button__SemiQuantitativePerfusion(id	, value='Semi-quantitative (Pixel)')
	Sid = PMI__Button__SemiQuantitativePerfusionRoi(id, value='Semi-quantitative (ROI)')
	Sid = PMI__Button__FastDeconvolutionAnalysisPVcorr(id, value='Model-free (Pixel)',/separator)
	Sid = PMI__Button__DeconvolutionAnalysisRoiPVcorr(id, value='Model-free (ROI)')
	Sid = PMI__Button__FitSingleInletPatlakPVcorr(id, value='Model-based (Pixel)',/separator)
	Sid = PMI__Button__FitSingleInletRoiPVcorr(id, value='Model-based (ROI)')

	id = widget_button(parent, value='Cardiac',/menu)

	Sid = PMI__Button__ExtractTimeWindow(id,value='Extract time window')
	Sid = PMI__Button__ArshadDeconvolution(id,value='DCE: Model-free (Pixel)',/separator)
	Sid = PMI__Button__ArshadDeconvolutionIRF(id,value='DCE: Model-free + IRF (pixel)')
	Sid = PMI__Button__ArshadDeconvolutionRoi(id,value='DCE: Model-free (ROI)',/separator)
	Sid = PMI__Button__ArshadFermiRoi(id,value='DCE: Fermi model (ROI - nonlinear)')
	Sid = PMI__Button__CardiacFermiRoi(id,value='DCE: Fermi model (ROI - linear)')
	Sid = PMI__Button__CardiacSingleInletRoiSlider(id, value='DCE: Compartment models (ROI)')
	Sid = PMI__Button__PulseWaveVelocityTime(id, value = 'PWV: Foot-to-foot delay',/separator)
	Sid = PMI__Button__PulseWaveForwardBackward(id, value = 'PWV: Forward/backward flow')

	id = widget_button(parent, value='Kidney',/menu)

	Sid = PMI__Button__KidneyModelsROI(id)
	Sid = PMI__Button__ToftsKidneyModelRoi(id)
	Sid = PMI__Button__KidneyFiltrationRoiStdAif(id, value = 'Filtration model with Parker AIF (ROI)')
	Sid = PMI__Button__FastDeconvolutionAnalysisTrig(id,value='Pixel-by-pixel deconvolution (with triggering)')
	Sid = PMI__Button__TriggeredKidneyFiltrationRoi(id, value='ROI filtration model (with triggering)')
	Sid = PMI__Button__FitPatlakLinearRoi(id, value='Fit Linearized Patlak (ROI)')

    id = widget_button(parent, value='Marco',/menu)

    Sid = PMI__Button__SemiQuantitativePerfusion(id	, value='Semi-quantitative (Pixel)')

    Sid = PMI__Button__MarcoPatlakPixel(id, value='Patlak (Pixel)',/separator)
    Sid = PMI__Button__MarcoExtPatlakPixel(id, value='Extended Patlak (Pixel)')
    Sid = PMI__Button__MarcoPatlakRoi(id, value='Patlak (ROI)', /separator)
    Sid = PMI__Button__MarcoExtPatlakRoi(id, value='Extended Patlak (ROI)')

	id = widget_button(parent, value='Other',/menu)

	Sid = PMI__Button__ExportLiverInterobserver(id)
	Sid = PMI__Button__DumpDicom(id)




end
