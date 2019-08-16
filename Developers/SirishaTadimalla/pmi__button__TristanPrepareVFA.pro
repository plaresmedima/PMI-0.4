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



pro PMI__Button__Event__TristanPrepareVFA, ev

	PMI__info, ev.top, Status=status, Series=Series, Stdy=Stdy


	dim = Series->d()
	x = [0,dim[0]-1]
	y = [0,dim[1]-1]
	z = [0,dim[2]-1]
	t = [0,dim[3]-1]

	d = 1 + [x[1]-x[0],y[1]-y[0],z[1]-z[0],t[1]-t[0]]

	FOR j=0L,dim[3]-1 DO BEGIN
		Win = Stdy -> New('SERIES', Default = Series, Name = Series->name()+'[FA]'+ string(round(j)), $
		Domain	= {z:Series->z(z[0],z[1]), t:Series->t(0), m:d[0:1]} )
		PMI__Message, status, 'Extracting', j/(dim[3]-1.0)
		for i=0L,d[2]-1 do begin
			im = Series -> Read(Stdy->DataPath(),z[0]+i,t[j])
		Win -> Write, Stdy->DataPath(), im[x[0]:x[1],y[0]:y[1]],j
	ENDFOR

	PMI__control, ev.top, /refresh
end


pro PMI__Button__Control__TristanPrepareVFA, id, v

	PMI__Info, tlb(id), Series=Series
	widget_control, id, sensitive = obj_valid(Series)
end

function PMI__Button__TristanPrepareVFA $
, 	parent 	$
,	separator = separator  $
,   value = value

	id = widget_button(parent $
	,	separator = separator $
	,   value = value $
	,	event_pro = 'PMI__Button__Event__TristanPrepareVFA'	$
	,	pro_set_value = 'PMI__Button__Control__TristanPrepareVFA' )

	return, id

end
