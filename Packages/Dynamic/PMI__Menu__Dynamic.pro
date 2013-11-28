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

pro PMI__Menu__Dynamic, parent

	id = widget_button(parent, value='Dynamic',/menu)

	Sid = PMI__Button__DynamicFastCollapse(id)
	Sid = PMI__Button__DynamicEnhancement(id)

	Sid = PMI__Button__DynamicDerivative(id,/separator)
	Sid = PMI__Button__DynamicSmooth(id)
	Sid = PMI__Button__DynamicRegrid(id,/separator)
;	Sid = PMI__Button__DynamicTriggerInternal(id)

	Sid = PMI__Button__ExtractTimeWindow(id,/separator)

;	Sid = PMI__Button__DecimateDynamic(id)
;	Sid = PMI__Button__InterpolateDynamics(id)
;	Sid = PMI__Button__Cluster(id)
;	sid = pmi__button__autocorrelation(id)
;   sid = pmi__button__crosscorrelation(id)

;	Sid = widget_button(id, value='Normalize',/menu,/separator)
;
;	SSid = PMI__Button__NormalizeDynamicToMaximum(Sid)
;	SSid = PMI__Button__NormalizeDynamicToIntegral(Sid)

end
