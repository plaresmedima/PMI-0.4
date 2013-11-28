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


pro PMI__Button__Event__RegionCombine, ev

	PMI__info, ev.top, Status=Status, Stdy=Stdy

 	in = PMI__Form(ev.top, Title='Combine Regions..', [$
	ptr_new({Type:'LIST', Tag:'ind', Label:'Regions to Combine', select:Stdy->sel(1), Value:Stdy->names(1)}) ])
	IF in.cancel THEN return

	Region0	= Stdy -> obj(1,in.ind[0])
	Bin = Stdy -> New('REGION'$
	,	Name 	= 'Combine['+Region0->Name() + '...]' $
	, 	Domain 	= Region0->dom() $
	,	Color 	= Region0->clr())

	d = Region0 -> d()
	n = n_elements(in.ind)

	for k=1L,d[2]*d[3]-1 do begin

		PMI__Message, status, 'Calculating', k/(d[2]*d[3]-1.0)

		im = Region0 -> Read(Stdy->DataPath(),k)

		for i=1L,n-1 do begin
			Regioni	= Stdy -> obj(1,in.ind[i])
			di = Regioni -> d()
			if total(d eq di) eq 4 then im = im or Regioni -> Read(Stdy->DataPath(),k)
		endfor

		if total(im) gt 0 then Bin -> Write, Stdy->DataPath(), im, k
	endfor

	PMI__control, ev.top, /refresh
end


pro PMI__Button__Control__RegionCombine, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	sensitive = 0
	if obj_valid(Stdy) then sensitive = Stdy->n(1) ge 2
	widget_control, id, sensitive = sensitive
end

function PMI__Button__RegionCombine, parent, separator=separator

  	id = widget_button(parent, $
  		separator = Separator,	$
  		value = 'Combine..', $
  		pro_set_value = 'PMI__Button__Control__RegionCombine', $
		event_pro  = 'PMI__Button__Event__RegionCombine')

	return, id
end
