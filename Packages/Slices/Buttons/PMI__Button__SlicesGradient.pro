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
pro PMI__Button__Event__SlicesGradient, ev

	PMI__info, ev.top, Status=Status, Stdy=Stdy, Series=Series

  	Grad = Stdy -> New('SERIES',Default=Series, Name= Series->name() +'[Grad]' )

	d = Series->d()
	FOR k=0L,d[2]*d[3]-1 DO BEGIN
		PMI__Message, status, 'Calculating '+Series->name(), k/(d[2]*d[3]-1E)
		im = Series -> Read(Stdy->DataPath(),k)
		im = PMI__Gradient(im)
		Grad -> Write, Stdy->DataPath(), im, k
	ENDFOR

	PMI__control, ev.top, /refresh
end






PRO PMI__Button__Control__SlicesGradient, id, v

	PMI__Info, tlb(id), Series=Series
    widget_control, id, sensitive=obj_valid(Series)
END





FUNCTION PMI__Button__SlicesGradient, $
	parent,$
	separator=separator,$
	value=value

	IF n_elements(value) EQ 0 THEN value='Image Gradient'

	RETURN, widget_button(parent,$
	 	value = value,$
	 	event_pro = 'PMI__Button__Event__SlicesGradient', $
	  	pro_set_value =  'PMI__Button__Control__SlicesGradient', $
		separator =	separator)
END
