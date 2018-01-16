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

	id = widget_button(parent, value='TACE Treatment Evaluation',/menu)
	Sid = PMI__Button__SemiQuantitativePerfusionN(id	, value='Semi-quantitative (Pixel)')
	Sid = PMI__Button__SemiQuantitativePerfusion100sec(id, value='Semi-quantitative over 100sec (Pixel)')

   	Sid = PMI__Button__FastDeconvolutionAnalysisN(id, value='Model-free (Pixel)',/separator)

	Sid = widget_button(id,value='Dual-inlet two compartment model', /menu, /separator)
	SSid = PMI__Button__FitDualInletRoiN(Sid, value = 'ROI-based')
	SSid = PMI__Button__DualInletUptakePixel(Sid, value='Pixel-based')


end
