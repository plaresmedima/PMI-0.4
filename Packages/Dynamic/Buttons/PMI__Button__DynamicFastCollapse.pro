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
function PMI__Button__Input__DynamicFastCollapse, top, nt, t

	t = [0,nt-1]

	While 1 do begin

		v = PMI__Form(top, Title='Dynamic range to collapse' $
		,	[ 	ptr_new({Type:'VALUE', Tag:'t0', Label:'First dynamic', Value:t[0]}) $
			,	ptr_new({Type:'VALUE', Tag:'t1', Label:'Last dynamic' , Value:t[1]})] $
		) & if v.cancel then return, 0

		t = [v.t0,v.t1]

		;Check which values are out of range
		Err = $
		[ (t[0] lt 0) or (t[0] ge nt) or (t[0] ge t[1])$
		, (t[1] lt 0) or (t[1] ge nt) or (t[1] le t[0])]

		i = where(Err,n) & if n eq 0 then return, 1
		msg = ['Selected values are outside of range','Please select a different range']
		if 'Cancel' eq dialog_message(msg,/information,/cancel) then return, 0

		;Reset values that are out of range
		for j=0L,n-1 do $
			Case i[j] of
			0:t[0] = 0
			1:t[1] = nt-1
			endcase
	endwhile
end




pro PMI__Button__Event__DynamicFastCollapse, ev

	PMI__Info, ev.top, Status=Status, Stdy=Stdy, Series=Data

	if not PMI__Button__Input__DynamicFastCollapse(ev.top,Data->d(3),t) then return

	widget_control, ev.id, get_value=label

	Series = Stdy -> New('SERIES' $
	,	name 	= Data->name()+'['+label+']' $
	,	Default	= Data )
	Series->t, Data->t(0)

	time = Data->t() - Data->t(0)
	time = time[t[0]:t[1]]

	d = Data->d()
	nt = t[1]-t[0]+1 ; number of time samples
	im = fltarr(d[0]*d[1],nt) ;2D-Array: [x*y,t]
	resized_time = transpose(rebin(time,nt,d[0]*d[1]))

	for i=0L,d[2]-1 do begin ;the slices loop

		PMI__Message, status, 'Calculating  ', i/(d[2]-1E)

		for j=t[0],t[1] do im[*,j-t[0]] = Data -> Read(Stdy->DataPath(), i,j)

		CASE label OF

			'Maximum':			slice = max(im,DIMENSION=2)
			'Minimum':			slice = min(im,DIMENSION=2)
			'Integral':			slice = IntArray(time,im)
			'Mean':				slice = total(im,2)/nt
			'1-norm':			slice = total(abs(im),2)
			'2-norm':			slice = sqrt(total(im^2,2))
			'Maximum norm':		slice = max(abs(im),DIMENSION=2)
			'First moment':		slice = IntArray(time,resized_time*im)
			'Second moment':	slice = IntArray(time,resized_time*im)
			'Time to minimum':	BEGIN
								tmp = min(im,ind, DIMENSION=2)
								slice = resized_time[ind]
								END
			'Time to maximum':	BEGIN
								tmp = max(im,ind, DIMENSION=2)
								slice = resized_time[ind]
								END
			'Standard deviation':BEGIN
								mean = total(im,2)/nt
								mean = rebin(mean,d[0]*d[1],nt)
								slice = sqrt(total((im-mean)^2,2)/nt)
								END
		ENDCASE

		Series -> Write, Stdy->DataPath(), slice, i
	endfor

	PMI__control, ev.top, /refresh
end

pro PMI__Button__Control__DynamicFastCollapse, id, v

	PMI__Info, tlb(id), Series=Series
	sensitive = 0
	if obj_valid(Series) then sensitive = Series->d(3) gt 1
    widget_control, id, sensitive=sensitive
end

function PMI__Button__DynamicFastCollapse, parent,value=value, separator=separator

	if n_elements(value) eq 0 then value = 'Parameters'

	id 	= widget_button(parent,/menu, separator=separator $
	,	value 		= value $
	,	event_pro 	= 'PMI__Button__Event__DynamicFastCollapse'	)

	Sid = widget_button(id, value = 'Mean'				, pro_set_value='PMI__Button__Control__DynamicFastCollapse')
	Sid = widget_button(id, value = 'Standard deviation', pro_set_value='PMI__Button__Control__DynamicFastCollapse')
	Sid = widget_button(id, value = 'Maximum'			, pro_set_value='PMI__Button__Control__DynamicFastCollapse',/separator)
	Sid = widget_button(id, value = 'Minimum'			, pro_set_value='PMI__Button__Control__DynamicFastCollapse')
	Sid = widget_button(id, value = 'Time to maximum'	, pro_set_value='PMI__Button__Control__DynamicFastCollapse')
	Sid = widget_button(id, value = 'Time to minimum'	, pro_set_value='PMI__Button__Control__DynamicFastCollapse')
	Sid = widget_button(id, value = 'Integral'			, pro_set_value='PMI__Button__Control__DynamicFastCollapse',/separator)
	Sid = widget_button(id, value = 'First moment'		, pro_set_value='PMI__Button__Control__DynamicFastCollapse')
	Sid = widget_button(id, value = 'Second moment'		, pro_set_value='PMI__Button__Control__DynamicFastCollapse')
	Sid = widget_button(id, value = '1-norm'			, pro_set_value='PMI__Button__Control__DynamicFastCollapse',/separator)
	Sid = widget_button(id, value = '2-norm'			, pro_set_value='PMI__Button__Control__DynamicFastCollapse')
	Sid = widget_button(id, value = 'Maximum norm'		, pro_set_value='PMI__Button__Control__DynamicFastCollapse')

	return, id
end
