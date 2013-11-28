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

pro PMI__Button__Event__SlicesShift, ev

	PMI__info, ev.top, Status=Status, Stdy=Stdy, Series=Series

	v = PMI__Form(ev.top, Title='Shift slices..', [$
		ptr_new({Type:'DROPLIST', Tag:'dir', Label:'Shift..', Value:['Columns up', 'Columns down', 'Rows left', 'Rows right']}),$
		ptr_new({Type:'VALUE', Tag:'n', Label:'Number of pixels', Value:10L}) ])
		IF v.cancel THEN return

	case v.dir[0] of
		0: n = [0,+v.n]
		1: n = [0,-v.n]
		2: n = [-v.n,0]
		3: n = [+v.n,0]
	endcase

  	Shft = Stdy -> New('SERIES', $
  		ClrWin = Series -> Trim(), $
  		Default	= Series, $
  		Name = Series->name() +'[Shifted]' )

	d = Series -> d()

	for k=0L,d[2]*d[3]-1 do begin

		PMI__Message, status, 'Shifting '+Series->name(), k/(d[2]*d[3]-1E)
		im = Series -> Read(Stdy->DataPath(),k)
		Shft -> Write, Stdy->DataPath(), shift(im,n[0],n[1]), k
	endfor

	PMI__control, ev.top, /refresh
end



PRO PMI__Button__Control__SlicesShift, id, v

	PMI__Info, tlb(id), Series=Series
    widget_control, id, sensitive=obj_valid(Series)
END

function PMI__Button__SlicesShift, $
	parent,	$
	separator=separator,$
	value=value

	if n_elements(value) eq 0 then value='Shift'

	return, widget_button(parent, 	$
	 	value = value, $
		event_pro = 'PMI__Button__Event__SlicesShift', $
		pro_set_value =  'PMI__Button__Control__SlicesShift', $
		separator = separator )
end
