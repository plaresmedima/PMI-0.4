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

pro PMI__Button__Event__SlicesRotate, ev

	PMI__info, ev.top, Status=Status, Stdy=Stdy, Series=Series

	v = PMI__Form(ev.top, Title='Mirror..', [$
		ptr_new({Type:'DROPLIST', Tag:'interp', Label:'Interpolate?', Value:['No','Yes'], Select:1}),$
		ptr_new({Type:'VALUE', Tag:'angle', Label:'Clockwise rotation angle (deg)', Value:90E}) ])
		IF v.cancel THEN return

	d = Series -> d()

  	New = Stdy -> New('SERIES', $
  		ClrWin  = Series -> Trim(), $
  		Default	= Series, $
  	 	Name	= Series->name() +'[Rot '+strcompress(v.angle,/remove_all)+']')

	FOR k=0L,d[2]*d[3]-1 DO BEGIN
		PMI__Message, status, 'Rotating', k/(d[2]*d[3]-1E)
		im = Series -> Read(Stdy->DataPath(),k)
		im = Rot(im,v.angle,interp=v.interp)
		New -> Write, Stdy->DataPath(), im, k
	ENDFOR

	PMI__control, ev.top, /refresh
end

PRO PMI__Button__Control__SlicesRotate, id, v

	PMI__Info, tlb(id), Series=Series
    widget_control, id, sensitive=obj_valid(Series)
END


function PMI__Button__SlicesRotate,$
 	parent,$
 	separator=separator,$
 	value=value

	if n_elements(value) eq 0 then value='Rotate'

	RETURN, widget_button(parent,$
	 	value = value,$
		event_pro = 'PMI__Button__Event__SlicesRotate',$
		pro_set_value = 'PMI__Button__Control__SlicesMirror',$
		separator =	separator)
end
