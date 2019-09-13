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



pro PMI__Menu__Cathy, parent

	PMI__Menu__Skeleton, parent
	PMI__Menu__Slices, parent
	PMI__Menu__Dynamic, parent

    id = widget_button(parent, value='TRISTAN Rat MSD',/menu)

;	Sid = PMI__Button__SeriesImportTristanRatDicom(id, value='Import DICOMs')
	Sid = PMI__Button__TristanSemiQuantitativePerfusion(id	, value='Create time-MIPs')
	Sid = PMI__Button__TristanPrepareVFA(id, value='Prepare VFA data for fitting')
	Sid = PMI__Button__TRISTAN_LinearVFA_T1mapping(id, value='Variable Flip Angle T1-mapping (Pixel)')
;	Sid = PMI__Button__tristanratsroi(id, value='Fit Rat Gadoxetate model v1.0')
;	Sid = PMI__Button__tristanratsroi_v1_1(id, value='Fit Rat Gadoxetate model v1.1')
	Sid = PMI__Button__tristanratsroi_v1_2(id, value='Fit Rat Gadoxetate model v1.2')

end
