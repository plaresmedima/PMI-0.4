pro PMI__Menu__JoeOther, parent

	id = widget_button(parent, value='Other',/menu)

	Sid = PMI__Button__ExtractTimeWindow(id)
	Sid = PMI__Button__SeriesExportChecker(id)
	Sid = PMI__Button__SlicesGradient(id)
	Sid = PMI__Button__RegionSelectByRangeInteractive(id,/separator)
	Sid = PMI__Button__DumpDicom(id)

end

;pro PMI__Menu__JoePerfusion, parent
;
;	id = widget_button(parent, value='Perfusion',/menu)
;
;	Sid = PMI__Button__SemiQuantitativePerfusion(id	, value='Semi-quantitative (Pixel)')
;	Sid = PMI__Button__SemiQuantitativePerfusionRoi(id, value='Semi-quantitative (ROI)')
;
;	Sid = widget_button(id,value='Model-free analysis', /menu, /separator)
;
;	SSid = PMI__Button__FastDeconvolutionAnalysis(Sid, value='Deconvolution (Pixel)')
;	SSid = PMI__Button__VolumeOfDistribution(Sid, value='Volume of distribution (Pixel)')
;	SSid = PMI__Button__DeconvolutionAnalysisRoi(Sid, value='Deconvolution (ROI)')
;	SSid = PMI__Button__MaximumSlopeRoi(Sid, value='Maximum slope (ROI)')
;
;	Sid = PMI__Button__FitSingleInletRoi(id	, value='All single-inlet models (ROI)')
;
;	Sid = widget_button(id,value='2 compartments (ROI)', /menu, /separator)
;
;	SSid = PMI__Button__FitAATHRoi(Sid							, value='AATH')
;	SSid = PMI__Button__FitExchangeRoi(Sid						, value='Exchange')
;	SSid = PMI__Button__FitSingleInletUptakeExchangeRoi(Sid		, value='Uptake')
;	SSid = PMI__Button__FitToftsRoi(Sid							, value='Modified Tofts')
;	SSid = PMI__Button__FitPatlakRoi(Sid						, value='Patlak')
;
;	Sid = widget_button(id,value='2 compartments with delay (ROI)', /menu)
;
;	SSid = PMI__Button__FitAATHDelayRoi(Sid							, value='AATH')
;	SSid = PMI__Button__FitExchangeDelayRoi(Sid						, value='Exchange')
;	SSid = PMI__Button__FitSingleInletUptakeExchangeDelayRoi(Sid	, value='Uptake')
;	SSid = PMI__Button__FitToftsDelayRoi(Sid						, value='Modified Tofts')
;	SSid = PMI__Button__FitPatlakDelayRoi(Sid						, value='Patlak')
;
;	Sid = widget_button(id,value='2 compartments (Pixel)', /menu)
;
;	SSid = PMI__Button__FitExchange(Sid			, value='Exchange')
;	SSid = PMI__Button__FitTofts(Sid			, value='Modified Tofts')
;	SSid = PMI__Button__FitPatlak(Sid			, value='Patlak')
;
;	Sid = widget_button(id,value='1 compartment (ROI)', /menu , /separator)
;
;	SSid = PMI__Button__OneCompartmentRoi(Sid		, value='Without delay or dispersion')
;	SSid = PMI__Button__FitKetyDelRoi(Sid			, value='With delay')
;	SSid = PMI__Button__FitOneCompDispRoi(Sid		, value='With dispersion' )
;	SSid = PMI__Button__FitOneCompDispDelRoi(Sid	, value='With delay and dispersion')
;
;	Sid = widget_button(id,value='1 compartment (Pixel)', /menu)
;
;	SSid = PMI__Button__FitKety(Sid			, value='Without delay or dispersion')
;	SSid = PMI__Button__FitOneCompDel(Sid	, value='With delay')
;	SSid = PMI__Button__FitOneCompDisp(Sid	, value='With dispersion' )
;
;end

pro PMI__Menu__Joe, parent

	PMI__Menu__Study, parent
	PMI__Menu__Series, parent
	PMI__Menu__Region, parent
	PMI__Menu__Window, parent
	PMI__Menu__Slices, parent
	PMI__Menu__Dynamic, parent
	PMI__Menu__Perfusion, parent
	PMI__Menu__JoeOther, parent

end
