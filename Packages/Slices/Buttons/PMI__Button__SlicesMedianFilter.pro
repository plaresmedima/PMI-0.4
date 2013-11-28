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

pro PMI__Button__Event__SlicesMedianFilter, ev

	PMI__info, ev.top, Status=Status, Stdy=Stdy

	Series  = Stdy->Names(0,ns)
	Regions = ['<entire image>',Stdy->Names(1,nr)]

	v = PMI__Form(ev.top, Title='Median Filter', [$
		ptr_new({Type:'DROPLIST', Tag:'series', Label:'Filter series', Value:Series, Select:Stdy->sel(0)}),$
		ptr_new({Type:'DROPLIST', Tag:'region', Label:'Filter pixels in', Value:Regions, Select:0}),$
		ptr_new({Type:'VALUE', Tag:'win', Label:'Filtering window', Value:3L}) ])
		IF v.cancel THEN return

	Series = Stdy->Obj(0,v.series)
	IF v.region GT 0 then Roi = Stdy->Obj(1,v.region-1)

 	Filtered = Stdy->New('SERIES',Default=Series, Name=Series->name() +'[Filtered]')

	win = 2*(v.win/2) + 1
	d = Series -> d()
	for k=0L,d[2]*d[3]-1 do begin

		PMI__Message, status, 'Filtering '+Series->name(), k/(d[2]*d[3]-1E)

		im = Series -> Read(Stdy->DataPath(),k)
		IF obj_valid(Roi) THEN BEGIN
			ind = Roi -> Where(Stdy->DataPath(),k,n=n)
			IF n GT 0 THEN im = MedianFilterROI(im,ind,win)
		endif else im = MedianFilter(im,win)

		Filtered -> Write, Stdy->DataPath(), im, k
	endfor

	PMI__control, ev.top, /refresh
end



PRO PMI__Button__Control__SlicesMedianFilter, id, v

	PMI__Info, tlb(id), Series=Series
    widget_control, id, sensitive=obj_valid(Series)
END

FUNCTION PMI__Button__SlicesMedianFilter,$
 	parent,$
 	separator=separator,$
 	value=value

	if n_elements(value) eq 0 then value='Median Filtering'

	RETURN, widget_button(parent, 	$
	 	value		= value, $
		event_pro	= 'PMI__Button__Event__SlicesMedianFilter', $
		pro_set_value =  'PMI__Button__Control__SlicesMedianFilter', $
		separator	= separator )

END
