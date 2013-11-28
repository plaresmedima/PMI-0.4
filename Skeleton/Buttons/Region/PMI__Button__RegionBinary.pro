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


pro PMI__Button__Event__RegionBinary, ev


	PMI__info, ev.top, Status=Status, Stdy=Stdy, Display=Display


	;GET INPUT AND CHECK CONSISTENCY


	op	= ['OR','AND','AND NOT']

	in = cw_inputform(/pos, ev=ev $
	,	Labels 			= ['Region 1'		,'Region 2'			,'Operation'				]$
	,	ListNames 		= [Stdy -> names(1)	,Stdy -> names(1)	,'Region 1 ' + op + ' Region 2'	]$
	,	ListNumbers 	= [Stdy -> n(1)		,Stdy -> n(1)		,3							]$
	,	ListDefaults 	= [Stdy -> sel(1)	,Stdy -> sel(1)		,0L							])

	if size(in,/type) eq 1 then return

	op = op[in.select[2]]

	Region0	= Stdy 	-> obj(1,in.select[0])
	Region1	= Stdy 	-> obj(1,in.select[1])

	d0	= Region0 -> d()
	d1	= Region1 -> d()

	if total(d0 eq d1) ne 4 then begin
		msg 	= 'Please select regions with identical dimensions'
		ok 		= dialog_message(msg,/information)
		return
	endif



	;CALCULATE



	Bin = Stdy -> New('REGION'$
	,	Name 	= '['+Region0->name() +']'+ op + '['+Region1->name() +']'$
	, 	domain 	= Region0->dom() $
	,	color 	= Region0->clr())

	for k=0L,d0[2]*d0[3]-1 do begin

		PMI__Message, status, 'Calculating', k/(d0[2]*d0[3]-1.0)

		im0 = Region0 -> Read(Stdy->DataPath(),k)
		im1 = Region1 -> Read(Stdy->DataPath(),k)

		case op of
			'AND'		:im0 = im0 and im1
			'OR'		:im0 = im0 or im1
			'AND NOT'	:im0 = im0 and not im1
		endcase

		if total(im0) gt 0 then Bin -> Write, Stdy->DataPath(), im0, k
	endfor

	PMI__control, ev.top, /refresh
end


pro PMI__Button__Control__RegionBinary, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	sensitive = 0
	if obj_valid(Stdy) then sensitive = Stdy->n(1) ge 2
	widget_control, id, sensitive = sensitive
end

function PMI__Button__RegionBinary, parent, separator=separator

  	id = widget_button(parent $
  	, 	separator	= Separator	$
  	,	value 		= 'F(Region 1, Region 2)'$
  	,	pro_set_value = 'PMI__Button__Control__RegionBinary' $
	,	event_pro 	= 'PMI__Button__Event__RegionBinary')

	return, id
end
