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
PRO PMI__Button__Event__SlicesCollapse, ev

	PMI__Info, ev.top, Status=Status, Stdy=Stdy, Series=Data
	widget_control, ev.id, get_value=label

	Series = Stdy -> New('SERIES' $
	,	name 	= Data->name()+'['+label+']' $
	,	Default	= Data )
	Series->z, Data->z(0)

	d = Data->d()
	FOR j=0L,d[3]-1 DO BEGIN ;the dynamics loop

		PMI__Message, status, 'Calculating  ', j/(d[3]-1E)
		im = Data -> Read(Stdy->DataPath(), -1, j)
		im = reform(im,d[0]*d[1],d[2],/overwrite)

		CASE label OF
			'Maximum':	slice = max(im,DIMENSION=2)
			'Minimum':	slice = min(im,DIMENSION=2)
			'Mean':		slice = total(im,2)/d[2]
			'Standard deviation':BEGIN
						mean = total(im,2)/d[2]
						mean = rebin(mean,d[0]*d[1],d[2])
						slice = sqrt(total((im-mean)^2,2)/d[2])
						END
		ENDCASE
		Series -> Write, Stdy->DataPath(), slice, j
	ENDFOR
	PMI__control, ev.top, /refresh
END



pro PMI__Button__Control__SlicesCollapse, id, v

	PMI__Info, tlb(id), Series=Series
	sensitive = 0
	if obj_valid(Series) then sensitive = Series->d(2) gt 1
    widget_control, id, sensitive=sensitive
end

function PMI__Button__SlicesCollapse, $
	parent,$
	separator=separator,$
	value=value

	if n_elements(value) eq 0 then value = 'Parameters'

	id 	= widget_button(parent,/menu, separator=separator $
	,	value = value $
	,	event_pro = 'PMI__Button__Event__SlicesCollapse')

	Sid = widget_button(id, value = 'Mean'				, pro_set_value='PMI__Button__Control__SlicesCollapse')
	Sid = widget_button(id, value = 'Standard deviation', pro_set_value='PMI__Button__Control__SlicesCollapse')
	Sid = widget_button(id, value = 'Maximum'			, pro_set_value='PMI__Button__Control__SlicesCollapse',/separator)
	Sid = widget_button(id, value = 'Minimum'			, pro_set_value='PMI__Button__Control__SlicesCollapse')

	return, id
end
