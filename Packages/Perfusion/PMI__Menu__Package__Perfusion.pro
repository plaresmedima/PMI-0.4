
;DESCRIPTION:
;Buttons for generic perfusion analysis

;REQUIRED PACKAGES:
;Mpfit

;WRITTEN BY:
;Steven Sourbron, University of Leeds (s.sourbron@leeds.ac.uk)

;LAST MODIFIED:
;28-04-2011

;
;
;    Copyright (C) 2009 Steven Sourbron
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
;
;
;



pro PMI__Menu__Package__Perfusion, parent


id = widget_button(parent, value='Perfusion',/menu)

	Sid = PMI__Button__SemiQuantitativePerfusion(id	, value='Semi-quantitative (Pixel)')
	Sid = PMI__Button__SemiQuantitativePerfusionRoi(id, value='Semi-quantitative (ROI)')

	Sid = widget_button(id,value='Model-free (Pixel)', /menu, /separator)

		SSid = PMI__Button__FastDeconvolutionAnalysis(Sid, value='Deconvolution')
		SSid = PMI__Button__VolumeOfDistribution(Sid, value='Volume of distribution')

	Sid = widget_button(id,value='Model-free (ROI)', /menu)

		SSid = PMI__Button__DeconvolutionAnalysisRoi(Sid, value='Deconvolution')
		SSid = PMI__Button__MaximumSlopeRoi(Sid, value='Maximum slope')

	Sid = widget_button(id,value='Exchange models (Pixel)', /menu, /separator)

		SSid = PMI__Button__FitExchange(Sid, value='2 compartment')
		SSid = PMI__Button__FitSingleInletUptakeExchange(Sid, value='2-compartment uptake')
		SSid = PMI__Button__FitModifiedTofts(Sid, value='Modified Tofts')
		SSid = PMI__Button__FitPatlak(Sid, value='Patlak')
		SSid = PMI__Button__FitTofts(Sid, value='1 compartment')

	Sid = PMI__Button__FitSingleInletRoi(id, value='Exchange models (ROI)')


end
