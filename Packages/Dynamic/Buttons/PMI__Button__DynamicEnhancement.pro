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
pro PMI__Button__Event__DynamicEnhancement, ev

	PMI__Info, ev.top, status=status, Series=Series, Stdy=Stdy

	v = PMI__Form(ev.top, Title='Signal enhancement settings', $
		[ ptr_new({Type:'DROPLIST',Tag:'relative',Label: 'Relative to baseline?', Value:['No','Yes'], Select:1}) $
		, ptr_new({Type:'VALUE'	,Tag:'nbaseline',Label: 'Length of baseline (# of dynamics)', Value:5L}) ] $
		) & if v.cancel then return

	SE = Stdy->New('SERIES', Default=Series, Name=Series->name()+'[Enhancement]')
	d = Series->d()

	for i=0L,d[2]-1 do begin

		PMI__Message, Status, 'Calculating', i/(d[2]-1E)

		im0 = Series -> Read(Stdy->DataPath(), i,0)
		for k=1L,v.nbaseline-1 do im0 = im0 + Series -> Read(Stdy->DataPath(), i,k)
		im0 = im0/v.nbaseline

		for j=0L,d[3]-1 do begin
			im = Series->Read(Stdy->DataPath(), i,j) - im0
			if v.relative then im = im/im0
			SE -> Write, Stdy->DataPath(), remove_inf(im), i,j
		endfor
	endfor

	PMI__Control, ev.top, /refresh
end

pro PMI__Button__Control__DynamicEnhancement, id, v

	PMI__Info, tlb(id), Series=Series
	sensitive = 0
	if obj_valid(Series) then sensitive = Series->d(3) gt 1
    widget_control, id, sensitive=sensitive
end

function PMI__Button__DynamicEnhancement, parent, value=value, separator=separator

	if n_elements(value) eq 0 then value = 'Enhancement'

	id = widget_button(parent $
	,	separator	= separator $
	, 	value 		= value $
	, 	event_pro 	= 'PMI__Button__Event__DynamicEnhancement' $
	,  	pro_set_value= 'PMI__Button__Control__DynamicEnhancement' $
	)

	return, id
end
