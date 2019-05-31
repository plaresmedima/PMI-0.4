;Menu for the demo version on
;https://sites.google.com/site/plaresmedima/

;REQUIRES PACKAGES:
;  Slices
;  Dynamic
;  Perfusion
;  MoCoMo

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



pro PMI__Menu__TRISTAN, parent

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

    id = widget_button(parent, value='MoCoMo',/menu)

    Sid = PMI__Button__MOCOMO_3D_DCE_2CM(id, value='3D DCE 2CM')
    Sid = PMI__Button__MOCOMO_3D_CONST(id, value='3D Constant')
    Sid = PMI__Button__MOCOMO_3D_VFA(id, value='3D VFA')
    Sid = PMI__Button__MOCOMO_2D_DCE_2CM(id, value='2D DCE 2CM', /separator)
    Sid = PMI__Button__MOCOMO_2D_DCE_1CM(id, value='2D DCE 1CM')
    Sid = PMI__Button__MOCOMO_2D_DCE_1CMD(id, value='2D DCE 1CM + Delay')
    Sid = PMI__Button__MOCOMO_2D_CONST(id, value='2D Constant')

    id = widget_button(parent, value='TRISTAN',/menu)

	Sid = PMI__Button__TRISTAN_Import(id, value = 'Import DICOM')
	Sid = PMI__Button__TRISTAN_LinearVFA_T1mapping(id, value='Variable Flip Angle T1-mapping (Pixel)')
	Sid = PMI__Button__TRISTAN_MOLLI_T1mapping(id, value='MOLLI T1-mapping (Pixel)')
	Sid = PMI__Button__FitT1SaturationRecoveryPixel(id, value='Saturation Recovery T1-mapping (Pixel)')
	Sid = PMI__Button__DumpDicom(id,/separator)

end
