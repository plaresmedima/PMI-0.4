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


pro PMI__Button__Event__SeriesDuplicate, ev

	PMI__info, ev.top, Stdy=Stdy, Series=Series, status=status

	Dupl = Stdy -> New('SERIES' $
	,	Default = Series $
	, 	Name = 'copy_of_' + Series->name() $
	)

	d = Series->d()

	for k=0L,d[2]*d[3]-1 do begin

		PMI__Message, status, 'Copying data', k/(d[2]*d[3]-1E)
		Dupl->Write, Stdy->DataPath(), Series->Read(Stdy->DataPath(),k), k
	endfor

	PMI__control, ev.top, /refresh
end


pro PMI__Button__Control__SeriesDuplicate, id, v

	PMI__Info, tlb(id), Series=Series
	widget_control, id, sensitive = obj_valid(Series)
end
function PMI__Button__SeriesDuplicate, parent

	return, widget_button(parent $
	, 	value = 'Duplicate' $
	, 	event_pro = 'PMI__Button__Event__SeriesDuplicate'	$
	,	pro_set_value = 'PMI__Button__Control__SeriesDuplicate' $
	)

end