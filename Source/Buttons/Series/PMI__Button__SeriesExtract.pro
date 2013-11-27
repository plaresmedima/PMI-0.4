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


function PMI__Button__Input__SeriesExtract, top, d, x, y, z, t

	x = [0,d[0]-1]
	y = [0,d[1]-1]
	z = [0,d[2]-1]
	t = [0,d[3]-1]

	While 1 do begin

		v = PMI__Form(top, Title='Extract series', [$
			ptr_new({Type:'VALUE', Tag:'x0', Label:'Take x from index'	, Value:x[0]}),$
			ptr_new({Type:'VALUE', Tag:'x1', Label:'Take x until index' , Value:x[1]}),$
			ptr_new({Type:'VALUE', Tag:'y0', Label:'Take y from index'	, Value:y[0]}),$
			ptr_new({Type:'VALUE', Tag:'y1', Label:'Take y until index' , Value:y[1]}),$
			ptr_new({Type:'VALUE', Tag:'z0', Label:'Take z from index'	, Value:z[0]}),$
			ptr_new({Type:'VALUE', Tag:'z1', Label:'Take z until index' , Value:z[1]}),$
			ptr_new({Type:'VALUE', Tag:'t0', Label:'Take t from index'	, Value:t[0]}),$
			ptr_new({Type:'VALUE', Tag:'t1', Label:'Take t until index' , Value:t[1]})])
			if v.cancel then return, 0

		x = [v.x0,v.x1]
		y = [v.y0,v.y1]
		z = [v.z0,v.z1]
		t = [v.t0,v.t1]

		;Check which values are out of range
		Err = [$
			(x[0] lt 0) or (x[0] ge d[0]) or (x[0] gt x[1]),$
		 	(x[1] lt 0) or (x[1] ge d[0]) or (x[1] lt x[0]),$
		 	(y[0] lt 0) or (y[0] ge d[1]) or (y[0] gt y[1]),$
		 	(y[1] lt 0) or (y[1] ge d[1]) or (y[1] lt y[0]),$
		 	(z[0] lt 0) or (z[0] ge d[2]) or (z[0] gt z[1]),$
		 	(z[1] lt 0) or (z[1] ge d[2]) or (z[1] lt z[0]),$
		 	(t[0] lt 0) or (t[0] ge d[3]) or (t[0] gt t[1]),$
			(t[1] lt 0) or (t[1] ge d[3]) or (t[1] lt t[0])]

		i = where(Err,n) & if n eq 0 then return, 1
		msg = ['Selected values are outside of range','Please select a different range']
		if 'Cancel' eq dialog_message(msg,/information,/cancel) then return, 0

		;Reset values that are out of range
		for j=0L,n-1 do $
			Case i[j] of
			0:x[0] = 0
			1:x[1] = d[0]-1
			2:y[0] = 0
			3:y[1] = d[1]-1
			4:z[0] = 0
			5:z[1] = d[2]-1
			6:t[0] = 0
			7:t[1] = d[3]-1
			endcase
	endwhile
end

pro PMI__Button__Event__SeriesExtract, ev

	PMI__info, ev.top, Status=status, Series=Series, Stdy=Stdy

	if not PMI__Button__Input__SeriesExtract(ev.top,Series->d(),x,y,z,t) then return

	d = 1 + [x[1]-x[0],y[1]-y[0],z[1]-z[0],t[1]-t[0]]

	Win = Stdy -> New('SERIES', $
		Default = Series, $
		Name 	= Series->name()+ '[Block]', $
		Domain	= {z:Series->z(z[0],z[1]), t:Series->t(t[0],t[1]), m:d[0:1]} )

	FOR j=0L,d[3]-1 DO BEGIN
		FOR i=0L,d[2]-1 DO BEGIN
			PMI__Message, status, 'Extracting', (j*d[2]+i)/(d[2]*d[3]-1.0)
			im = Series -> Read(Stdy->DataPath(),z[0]+i,t[0]+j)
			Win -> Write, Stdy->DataPath(), im[x[0]:x[1],y[0]:y[1]], i,j
		ENDFOR
	ENDFOR

	PMI__control, ev.top, /refresh
end


pro PMI__Button__Control__SeriesExtract, id, v

	PMI__Info, tlb(id), Series=Series
	widget_control, id, sensitive = obj_valid(Series)
end

function PMI__Button__SeriesExtract $
, 	parent 	$
,	separator = separator

	id = widget_button(parent $
	,	separator = separator $
	,	value = 'Extract...'$
	,	event_pro = 'PMI__Button__Event__SeriesExtract'	$
	,	pro_set_value = 'PMI__Button__Control__SeriesExtract' )

	return, id

end
