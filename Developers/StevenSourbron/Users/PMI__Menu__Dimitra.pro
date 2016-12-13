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



pro PMI__Menu__Dimitra, parent

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

    id = widget_button(parent, value='Dimitra',/menu)

    Sid = PMI__Button__FitFiltration(id, value='Filtration model (NLLS - Steven)')
    Sid = PMI__Button__FitFiltrationDimitraNLLS(id, value='Filtration model (NLLS)',/separator)
    Sid = PMI__Button__FitFiltrationDimitraLLS(id, value='Filtration model (LLS)')
    Sid = PMI__Button__FitFiltrationDimitraLLSmodelfit(id, value='Filtration model fit (LLS)',/separator)
    Sid = PMI__Button__FitModToftsDimitraLLSmodelfit(id, value='Modified Tofts model fit (LLS)')
    Sid = PMI__Button__DimitraKidneyModelsROI(id, value = 'All kidney models (ROI)', /separator)



end
