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

pro PMI__Menu__Slices, parent

	id = widget_button(parent, value='Slices',/menu)

	Sid = PMI__Button__SlicesCollapse(id)
	Sid = PMI__Button__SlicesSmooth(id,/separator)
	Sid = PMI__Button__SlicesMedianFilter(id)
	Sid = PMI__Button__SlicesGradient(id)
	Sid = PMI__Button__SlicesMask(id,/separator)
 	Sid = PMI__Button__SlicesResolution(id,/separator)
 	Sid = PMI__Button__SlicesResolutionAnisotropic(id)
	Sid = PMI__Button__SlicesShift(id,/separator)
 	Sid = PMI__Button__SlicesMirror(id)
; 	Sid = PMI__Button__SlicesRotate(id)

; 	Sid = PMI__Button__SlicesAxialToCoronal(id,/separator)
; 	Sid = PMI__Button__SlicesCoronalToSagittal(id)
; 	Sid = PMI__Button__SlicesSagittalToAxial(id)

;	Sid = widget_button(id, value = 'Fusion', /menu,/separator)
;	SSid = PMI__Button__FusionExportColorMix(Sid, value = 'Color')
;    SSid = PMI__Button__SeriesExportChecker(Sid, value = 'Checkerboard')


end
