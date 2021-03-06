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

pro PMI__Menu__Sergios, parent

	PMI__Menu__Skeleton, parent
	PMI__Menu__Slices, parent
	PMI__Menu__Dynamic, parent

	id = widget_button(parent, value='Sergios',/menu)

	Sid = PMI__Button__Sergios_SemiQuantitativePerfusion(id	, value='Semi-quantitative (Pixel)')
   	Sid = PMI__Button__Sergios_FastDeconvolutionAnalysis(id, value='Model-free quantitative (Pixel)')

   	Sid = PMI__Button__Sergios_DualInletUptakePixel_Orig(id, value='Dual-inlet uptake model (Pixel)',/separator)
   	Sid = PMI__Button__Sergios_DualInletUptakePixel_DualTR_linear(id, value='Dual-inlet uptake model, dual TR (Pixel)')
   	Sid = PMI__Button__Sergios_DualInletUptakePixel(id, value='Dual-inlet uptake model non-linear (Pixel)')
   	Sid = PMI__Button__Sergios_DualInletUptakePixel_DualTR(id, value='Dual-inlet uptake model non-linear, dual-TR (Pixel)')

	Sid = PMI__Button__Sergios_FitDualInletRoi(id, value = 'Dual-inlet models (ROI)', /separator)
	Sid = PMI__Button__Sergios_FitDualInletRoi_V2(id, value = 'Dual-inlet models dual-TR (ROI)')

end
