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


pro PMI__Button__Event__RegionByRange, ev

	PMI__info, ev.top, Series=Series, Stdy=Stdy, status=status, Display=Display

	in = PMI__Form(ev.top, Title='Select ROI by range', [$
		ptr_new({Type:'LIST',Tag:'on', Label:'Define region on', Value:['All images','Current image'], Select:1}), $
		ptr_new({Type:'VALUE'	,Tag:'max', Label:'Select pixels with value below:', Value:Series->trim(1)}),$
		ptr_new({Type:'VALUE'	,Tag:'min', Label:'Select pixels with value above:', Value:Series->trim(0)})])
		IF in.cancel THEN return

	Roi = Stdy->New('REGION' $
	,	Name = Series->Name() + '['+PMI__Round(in.min,2,/string)+'-'+PMI__Round(in.max,2,/string)+']' $
	,	Domain = Series->dom() $
	,	Color = Series->Clr(SAT='R'))

	d = Series->d()
	bin = bytarr(d[0]*d[1])

	if in.on eq 1 then begin
		Display -> GET, CursorPos=P
		k0 = reform_ind(d[2:3],vec=P[2:3])
		k1 = k0
	endif else begin
		k0 = 0L
		k1 = d[2]*d[3]-1
	endelse

 	for k=k0,k1 do begin

 		PMI__Message, Status, 'Creating ROI', (k-k0)/(k1-k0)

 		Im = Series->Read(Stdy->DataPath(),k)
 		ind = where( (Im ge in.min) and (Im le in.max), cnt)
 		if cnt gt 0 then begin
 			bin[ind] = 1
 			Roi -> Write, Stdy->DataPath(), bin, k
 			bin[ind] = 0
 		endif
 	endfor

	PMI__Control, ev.top, /refresh
end

pro PMI__Button__Control__RegionByRange, id, v

	PMI__Info, tlb(id), Series=Series
	widget_control, id, sensitive = obj_valid(Series)
end


function PMI__Button__RegionByRange, parent, separator=separator, value=value

	if n_elements(value) eq 0 then value='Select by range'

  	id = widget_button(parent					$
  	, 	separator	= separator 				$
  	, 	value 		= value			$
	,	pro_set_value = 'PMI__Button__Control__RegionByRange' $
	, 	event_pro	= 'PMI__Button__Event__RegionByRange'	)

	return, id
end