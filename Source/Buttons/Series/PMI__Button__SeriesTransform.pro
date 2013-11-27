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


pro PMI__Button__Event__SeriesTransform, ev


	;GET USER INPUT


	widget_control, ev.id, get_value = label

	if (label eq 'a * x') $
	or (label eq 'x - a') $
	or (label eq 'x / a') then begin

		in 	= cw_inputform(/pos, ev=ev, labels=['Value of the Parameter "a"?'], datadefaults={a:0E})
		if size(in,/type) eq 1 then return
		a = in.data.a
	endif


	;CALCULATE


	PMI__info, ev.top, Status=status, Series=Series, Stdy=Stdy
	New = Stdy -> New('SERIES', Name=Series->name()+'['+label+']', Domain=Series->dom())
	d = Series -> d()

	for k=0L,d[2]*d[3]-1 do begin

		PMI__Message, status, 'Calculating', k/(d[2]*d[3]-1.0)

		im = Series -> Read(Stdy->DataPath(),k)

		case label of
			'exp(x)'		:im = exp(im)
			'ln(x)'			:im = alog(im)
			'abs(x)'		:im = abs(im)
			'- x'			:im = - im
			'a * x'			:im = a*im
			'x - a'			:im = im - a
			'x / a'			:im = im / a
			'sqrt(x)'		:im = sqrt(im)
			'1:x'			:im = 1.0/im
			'finite(x)'		:
		endcase

		New -> Write, Stdy->DataPath(), remove_inf(im), k
	endfor

	return:PMI__control, ev.top, /refresh
end



pro PMI__Button__Control__SeriesTransform, id, v

	PMI__Info, tlb(id), Series=Series
	widget_control, id, sensitive = obj_valid(Series)
end


function PMI__Button__SeriesTransform 	$
, 	parent 	$
,	value = value $
,	separator = separator

	if n_elements(value) eq 0 then value = 'Transform'

  	id 	= widget_button(parent, /menu	$
  	,	separator = separator			$
  	,	value = value		$
  	,	event_pro = 'PMI__Button__Event__SeriesTransform')

	Sid = widget_button(id, value = 'exp(x)' 	,	pro_set_value = 'PMI__Button__Control__SeriesTransform')
	Sid = widget_button(id, value = 'ln(x)'		,	pro_set_value = 'PMI__Button__Control__SeriesTransform')
	Sid = widget_button(id, value = 'abs(x)'	,	pro_set_value = 'PMI__Button__Control__SeriesTransform')
	Sid = widget_button(id, value = '- x'		,	pro_set_value = 'PMI__Button__Control__SeriesTransform')
	Sid = widget_button(id, value = 'a * x'		,	pro_set_value = 'PMI__Button__Control__SeriesTransform')
	Sid = widget_button(id, value = 'x / a'		,	pro_set_value = 'PMI__Button__Control__SeriesTransform')
	Sid = widget_button(id, value = 'x - a'		,	pro_set_value = 'PMI__Button__Control__SeriesTransform')
	Sid = widget_button(id, value = 'sqrt(x)'	,	pro_set_value = 'PMI__Button__Control__SeriesTransform')
	Sid = widget_button(id, value = '1:x'		,	pro_set_value = 'PMI__Button__Control__SeriesTransform')
	Sid = widget_button(id, value = 'finite(x)'	,	pro_set_value = 'PMI__Button__Control__SeriesTransform')

	return, id
end
