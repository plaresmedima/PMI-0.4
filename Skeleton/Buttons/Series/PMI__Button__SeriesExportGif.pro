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


pro PMI__Button__Event__SeriesExportGif, ev

	PMI__info, ev.top, State=s, Series=data, Status=Status, Stdy=Stdy

	if not s -> get_file(file, title= 'Save images as', filter= '.gif') then return

	d = data->d()
	R = data->Clr(/R)
	G = data->Clr(/G)
	B = data->Clr(/B)

	for i=0L,d[2]-1 do begin
		for j=0L,d[3]-1 do begin

			PMI__Message, status, 'Exporting ', (i*d[3]+j)/(d[2]*d[3]-1.0)

			im = data -> image(Stdy->DataPath(),i,j,/clrstrip)
			write_gif, file, im, R, G, B, /MULTIPLE
		endfor
	endfor

	write_gif, file, /CLOSE

	PMI__Message, status
end

pro PMI__Button__Control__SeriesExportGif, id, v

	PMI__Info, tlb(id), Series=Series
	widget_control, id, sensitive = obj_valid(Series)
end
function PMI__Button__SeriesExportGif, parent, separator = separator

  	return, widget_button(parent $
  	, 	separator	= separator	$
  	, 	value		= 'animated GIF' $
	, 	event_pro 	= 'PMI__Button__Event__SeriesExportGif'$
	,	pro_set_value 	= 'PMI__Button__Control__SeriesExportGif' $
	)
end