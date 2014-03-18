pro PMI__Menu__PerfusionSteven, parent

	id = widget_button(parent, value='Perfusion',/menu)

	Sid = PMI__Button__SemiQuantitativePerfusion(id	, value='Semi-quantitative (Pixel)')
	Sid = PMI__Button__SemiQuantitativePerfusionRoi(id, value='Semi-quantitative (ROI)')

	Sid = widget_button(id,value='Model-free (Pixel)', /menu, /separator)

	SSid = PMI__Button__FastDeconvolutionAnalysis(Sid, value='Deconvolution (Fast)')
	SSid = PMI__Button__DeconvolutionAnalysis(Sid, value='Deconvolution (Accurate)')
	SSid = PMI__Button__VolumeOfDistribution(Sid, value='Volume of distribution', /separator)

	Sid = widget_button(id,value='Model-free (ROI)', /menu)

	SSid = PMI__Button__DeconvolutionAnalysisRoi(Sid, value='Deconvolution (Residue detection)')
	SSid = PMI__Button__DeconvolutionAnalysisRoiInletOutlet(Sid, value='Deconvolution (Outlet detection)')
	SSid = PMI__Button__MaximumSlopeRoi(Sid, value='Maximum slope perfusion')

	Sid = PMI__Button__FitSingleInletRoi(id	, value='All single-inlet models (ROI)', /separator)
	Sid = PMI__Button__FitDualInletRoi(id, value='All dual-inlet models (ROI)')

	Sid = widget_button(id,value='2 compartments (ROI)', /menu, /separator)

	SSid = PMI__Button__FitAATHRoi(Sid							, value='AATH')
	SSid = PMI__Button__FitExchangeRoi(Sid						, value='Exchange', /separator)
	SSid = PMI__Button__FitFiltrationRoi(Sid					, value='Filtration')
	SSid = PMI__Button__FitSingleInletUptakeExchangeRoi(Sid		, value='Uptake (Exchange)', /separator)
	SSid = PMI__Button__FitSingleInletUptakeFiltrationRoi(Sid	, value='Uptake (Filtration)')
	SSid = PMI__Button__FitToftsRoi(Sid							, value='Modified Tofts')
	SSid = PMI__Button__FitPatlakRoi(Sid						, value='Patlak', /separator)

	Sid = widget_button(id,value='2 compartments with delay (ROI)', /menu)

	SSid = PMI__Button__FitAATHDelayRoi(Sid							, value='AATH')
	SSid = PMI__Button__FitExchangeDelayRoi(Sid						, value='Exchange',/separator)
	SSid = PMI__Button__FitFiltrationDelayRoi(Sid					, value='Filtration')
	SSid = PMI__Button__FitSingleInletUptakeExchangeDelayRoi(Sid	, value='Uptake (Exchange)', /separator)
	SSid = PMI__Button__FitSingleInletUptakeFiltrationDelayRoi(Sid	, value='Uptake (Filtration)')
	SSid = PMI__Button__FitToftsDelayRoi(Sid						, value='Modified Tofts')
	SSid = PMI__Button__FitPatlakDelayRoi(Sid						, value='Patlak', /separator)

	Sid = widget_button(id,value='2 compartments (Pixel)', /menu)

	SSid = PMI__Button__FitExchange(Sid			, value='Exchange')
	SSid = PMI__Button__FitFiltration(Sid		, value='Filtration')
	SSid = PMI__Button__FitSingleInletUptakeExchange(Sid, value='Uptake (Exchange)',/separator)
	SSid = PMI__Button__FitSingleInletUptakeFiltration(Sid, value='Uptake (Filtration)')
	SSid = PMI__Button__FitModifiedTofts(Sid			, value='Modified Tofts')
	SSid = PMI__Button__FitPatlakLinPixel(Sid			, value='Patlak',/separator)


	Sid = widget_button(id,value='1 compartment (ROI)', /menu , /separator)

	SSid = PMI__Button__FitToftsRoi(Sid		, value='Without delay or dispersion')
	SSid = PMI__Button__FitToftsDelayRoi(Sid			, value='With delay',/separator  )
	SSid = PMI__Button__FitOneCompDispRoi(Sid		, value='With dispersion' )
	SSid = PMI__Button__FitOneCompDispDelRoi(Sid	, value='With delay and dispersion')

	Sid = widget_button(id,value='1 compartment (Pixel)', /menu)

	SSid = PMI__Button__FitTofts(Sid			, value='Without delay or dispersion')
	SSid = PMI__Button__FitOneCompDel(Sid	, value='With delay',/separator )
	SSid = PMI__Button__FitOneCompDisp(Sid	, value='With dispersion' )

	Sid = widget_button(id,value='Outlet detection', /menu ,/separator )

	SSid = PMI__Button__FitPureDelayRoi(Sid			, value='With delay (ROI)' )
	SSid = PMI__Button__FitPlugFlow(Sid				, value='With delay (Pixel)')
	SSid = PMI__Button__FitPlugFlowDispOutlet(Sid	, value='With delay and dispersion (ROI)',/separator)

	Sid = widget_button(id,value='Dual-inlet models (ROI)', /menu, /separator)

	SSid = PMI__Button__DualInletExchangeUptakeDualDelayRoi(Sid	, value='3-compartment exchange-uptake model with dual delay')
	SSid = PMI__Button__DualInletExchangeUptakeDelayRoi(Sid		, value='3-compartment exchange-uptake model with delay')
	SSid = PMI__Button__DualInletExchangeUptakeRoi(Sid			, value='3-compartment exchange-uptake model')
	SSid = PMI__Button__DualInletExchangeDualDelayRoi(Sid	, value='2-compartment exchange model with dual delay',/separator)
	SSid = PMI__Button__DualInletExchangeDelayRoi(Sid		, value='2-compartment exchange model with delay')
	SSid = PMI__Button__DualInletExchangeRoi(Sid			, value='2-compartment exchange model')
	SSid = PMI__Button__DualInletUptakeDualDelayRoi(Sid	, value='2-compartment uptake model with dual delay', /separator)
	SSid = PMI__Button__DualInletUptakeDelayRoi(Sid		, value='2-compartment uptake model with delay')
	SSid = PMI__Button__DualInletUptakeRoi(Sid			, value='2-compartment uptake model')
	SSid = PMI__Button__DualInletCompartmentDualDelayRoi(Sid	, value='1 compartment with dual delay',/separator)
	SSid = PMI__Button__DualInletCompartmentDelayRoi(Sid		, value='1 compartment with delay')
	SSid = PMI__Button__DualInletCompartmentRoi(Sid			, value='1 compartment')

	Sid = widget_button(id,value='Dual-inlet models (Pixel)', /menu)

	SSid = PMI__Button__DualInletCompartment(Sid, value='1 compartment')

	Sid = widget_button(id,value='Parametric deconvolution (ROI)', /menu)

	SSid = PMI__Button__FitFermiRoi(Sid,value='Fermi model')

end
