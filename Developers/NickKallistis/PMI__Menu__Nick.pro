;Menu for the demo version on
;https://sites.google.com/site/plaresmedima/

;REQUIRES PACKAGES:
;  Slices
;  Dynamic
;  Perfusion

;
;    Copyright (C) 2013 Steven Sourbron
;
;    This program is free software; you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation; either version 2 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License along
;    with this program; if not, write to the Free Software Foundation, Inc.,
;    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA


;TACE Treatment - Evaluation (collaboaration with Tubingen)

pro PMI__Menu__Nick, parent

	PMI__Menu__Skeleton, parent
	PMI__Menu__Slices, parent
	PMI__Menu__Dynamic, parent

	id = widget_button(parent, value='Cirrhosis',/menu)

	Sid = PMI__Button__Sergios_SemiQuantitativePerfusion(id	, value='Semi-quantitative analysis (Pixel)')
   	Sid = PMI__Button__Sergios_FastDeconvolutionAnalysis(id, value='Model-free quantitative analysis (Pixel)')
   	Sid = PMI__Button__NickLinearVFA(id, value='Variable Flip Angle T1-mapping (Pixel)')
   	Sid = PMI__Button__DualInletFiltrationPixel(id, value='Dual-inlet filtration model (Pixel)')
   	Sid = PMI__Button__Sergios_FitDualInletRoi(id, value = 'Dual-inlet models (ROI)')

	Sid = widget_button(id,value='Single-inlet two compartment model', /menu, /separator)
	SSid = PMI__Button__FitFiltrationDimitraLLS(Sid, value = 'Filtration Pixel-based - Linear Least Squares')

   	Sid = PMI__Button__Sergios_DualInletUptakePixel_Orig(id, value='Dual-inlet uptake model (Pixel)',/separator)
   	Sid = PMI__Button__Sergios_DualInletUptakePixel_delayAIF(id, value='Dual-inlet uptake model with delay AIF (Pixel)')
   	Sid = PMI__Button__Sergios_DualInletUptakePixel_DualTR_linear(id, value='Dual-inlet uptake model, dual TR (Pixel)')
   	Sid = PMI__Button__Sergios_DualInletUptakePixel(id, value='Dual-inlet uptake model non-linear (Pixel)')
   	Sid = PMI__Button__Sergios_DualInletUptakePixel_DualTR(id, value='Dual-inlet uptake model non-linear, dual-TR (Pixel)')
	Sid = PMI__Button__Sergios_FitDualInletRoi_V2(id, value = 'Dual-inlet models dual-TR (ROI)')


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

	SSid = PMI__Button__FitExchange(Sid, value='Exchange')
	SSid = PMI__Button__FitFiltration(Sid, value='Filtration')
	SSid = PMI__Button__FitSingleInletUptakeExchange(Sid, value='Uptake (Exchange)',/separator)
	SSid = PMI__Button__FitSingleInletUptakeFiltration(Sid, value='Uptake (Filtration)')
	SSid = PMI__Button__FitModifiedTofts(Sid, value='Modified Tofts')
;
	SSid = PMI__Button__FitPatlakLinPixel(Sid, value='Patlak',/separator)

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



;	Sid = PMI__Button__SemiQuantitativePerfusionN(id	, value='Semi-quantitative (Pixel)')
;	Sid = PMI__Button__SemiQuantitativePerfusion100sec(id, value='Semi-quantitative over 100sec (Pixel)')
;
;   Sid = PMI__Button__FastDeconvolutionAnalysisN(id, value='Model-free (Pixel)',/separator)
;
;	Sid = widget_button(parent,value='Dual-inlet two compartment model', /menu, /separator)
;	SSid = PMI__Button__FitDualInletRoiN(Sid, value = 'ROI-based')
;	SSid = PMI__Button__DualInletUptakePixel(Sid, value='Pixel-based')




end
