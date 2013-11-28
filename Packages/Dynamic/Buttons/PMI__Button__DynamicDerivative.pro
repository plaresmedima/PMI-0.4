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
pro PMI__Button__Event__DynamicDerivative, ev

	PMI__Info, ev.top, status=status, Series=Series, Stdy=Stdy

	New = Stdy->New('SERIES', $
	 	Name = Series->name() + '[Der]', $
	 	Default	= Series )

 	d = Series->d()
 	t = Series->t()

	for i=0L,d[2]-1 do begin

 		PMI__Message, Status, 'Calculating', i/(d[2]-1E)

		im = Series -> Read(Stdy->DataPath(), i,-1)
		im = reform(im,d[0]*d[1],d[3],/overwrite)
		FOR k=0L, d[0]*d[1]-1 DO BEGIN
			y = reform(im[k,*],/overwrite)
			im[k,*] = deriv(t,y)
		ENDFOR
		New -> Write, Stdy->DataPath(), im ,i ,-1
	endfor

	PMI__Control, ev.top, /refresh
end


pro PMI__Button__Control__DynamicDerivative, id, v

	PMI__Info, tlb(id), Series=Series
	sensitive = 0
	if obj_valid(Series) then sensitive = Series->d(3) gt 1
    widget_control, id, sensitive=sensitive
end

function PMI__Button__DynamicDerivative, $
	parent, $
	separator = separator

	return, widget_button(parent, $
		separator = separator, $
	 	value = 'Derive', $
	 	event_pro = 'PMI__Button__Event__DynamicDerivative', $
	  	pro_set_value= 'PMI__Button__Control__DynamicDerivative')
end
