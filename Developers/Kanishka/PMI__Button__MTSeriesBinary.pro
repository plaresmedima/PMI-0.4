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


function PMI__Button__Input__MTSeriesBinary, top, Stdy, Series0, Series1, Operation

    ; @KS: modified for iBEAt study MTR
	;"Operation" is the type of binary operation required (+,-,etc..)
	;"Series0" and "Series1" are the series in which the operation is performed
	;"Series0" and "Series1" must have the same dimensions


	sel = [Stdy->sel(0),Stdy->sel(0),0]

	while 1 do begin

		v = PMI__Form(top, Title='MTR: no moco', $
			[ ptr_new({Type:'DROPLIST',Tag:'s0',Label:'MT OFF Series', Value:Stdy -> names(0), Select:sel[0]}) $
			, ptr_new({Type:'DROPLIST',Tag:'s1',Label:'MT ON Series', Value:Stdy -> names(0), Select:sel[1]}) $
			]) & if v.cancel then return, 0

		sel = [v.s0,v.s1]
		Operation =  ['[-]']

		Series0 = Stdy -> obj(0,sel[0])
		Series1 = Stdy -> obj(0,sel[1])

		d0	= Series0 -> d()
		d1	= Series1 -> d()

		if total(d0 eq d1) eq 4 then return, 1
		msg = 'Please select images with identical dimensions'
		if 'Cancel' eq dialog_message(msg,/information,/cancel) then return, 0

	endwhile
end



pro PMI__Button__Event__MTSeriesBinary, ev

	PMI__info, ev.top, Status=Status, Stdy=Stdy

	if not PMI__Button__Input__MTSeriesBinary(ev.top,Stdy,Series0,Series1,Operation) then return

	Bin = Stdy -> New('SERIES' $
	,	Name ='MTR (no moco): ' + '['+Series0->name() +']'+ operation + '['+Series1->name() +']' $
	, 	domain = Series0->dom() )

	d0 = Series0 -> d()
	d1 = Series1 -> d()

	for k=0L,d0[2]*d0[3]-1 do begin

		PMI__Message, status, 'Calculating ', k/(d0[2]*d0[3]-1.0)

		im0 = Series0 -> Read(Stdy->DataPath(),k)
		im1 = Series1 -> Read(Stdy->DataPath(),k)

		case operation of
			'[-]':im0 = ((im0 - im1)/im0)*100 ; ((MT_OFF-MT_ON)/MT_OFF)*100
		endcase

		Bin -> Write, Stdy->DataPath(), remove_inf(im0), k
	endfor

	PMI__control, ev.top, /refresh
end



pro PMI__Button__Control__MTSeriesBinary, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	sensitive = 0
	if obj_valid(Stdy) then sensitive = Stdy->n(0) ge 2
	widget_control, id, sensitive = sensitive
end


function PMI__Button__MTSeriesBinary, parent, value=value, separator=separator

	if n_elements(value) eq 0 then value ='MTR: no moco'

  	id = widget_button(parent $
  	, 	separator=separator $
  	,	value = value	$
	,	event_pro = 'PMI__Button__Event__MTSeriesBinary'	$
	,	pro_set_value = 'PMI__Button__Control__MTSeriesBinary' )

	return, id
end
