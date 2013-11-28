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
;    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.



pro PMI__Button__Event__ViewRoiHistogram, ev

	PMI__info, ev.top, Series=Series, Region=Region, Stdy=Stdy, status=status

	v = PMI__RoiValues(Stdy->DataPath(), Series, Region, status, cnt=npix)
	if npix lt 2 then begin
		ok = dialog_message(/Information, 'You need more than two pixels in the ROI')
		return
	endif

	Display = PMI__DisplayNew(ev.top,'PMI__Display__ViewHistogram', Data=v, Series=Series->Name(), Region=Region->Name())
end


pro PMI__Button__Control__ViewRoiHistogram, id, v

	PMI__Info, tlb(id), Stdy=Stdy, Series=Series, Region=Region, Viewer=Viewer
	sensitive = obj_valid(Series) and obj_valid(Region)
    widget_control, id, sensitive=sensitive
end

function PMI__Button__ViewRoiHistogram, parent, separator=Separator, value=value

	if n_elements(value) eq 0 then value = 'ROI statistics'

	PMI__Display__ViewHistogram__Define

    id = widget_button(parent   $
    ,	value  = value  $
    ,  	event_pro  = 'PMI__Button__Event__ViewRoiHistogram'  $
   	,	pro_set_value = 'PMI__Button__Control__ViewRoiHistogram' $
    ,  	separator = Separator  $
    )
    return, id
end
