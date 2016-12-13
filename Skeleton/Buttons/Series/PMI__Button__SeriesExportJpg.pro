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


pro PMI__Button__Event__SeriesExportJpg, ev

	;GET FILENAME

	PMI__info, ev.top, State=s, Series=data, Status=Status, Stdy=Stdy

	if not s -> get_file(file, title= 'Save images as', filter= '.jpeg') then return
	file = strmid(file,0,strlen(file)-strlen('.jpeg'))

	;SAVE IMAGES

	d = data -> d ()

	for j=0L,d[3]-1 do begin
	for i=0L,d[2]-1 do begin

		PMI__Message, status, 'Exporting ', (j*d[2]+i)/(d[2]*d[3]-1.0)

		file_ij = file + '(' + strnr(i,3) + ',' + strnr(j,3) + ').jpeg'
		im 		= data -> image(Stdy->DataPath(),i,j,/clrstrip,/bits24)
		write_jpeg, file_ij, im, /true, quality=100
	endfor
	endfor

	PMI__Message, status
end


pro PMI__Button__Control__SeriesExportJpg, id, v

	PMI__Info, tlb(id), Series=Series
	widget_control, id, sensitive = obj_valid(Series)
end
function PMI__Button__SeriesExportJpg, parent, separator	= separator

  	return, widget_button(parent	$
  	, 	separator	= separator	$
  	, 	value		= 'JPEG' $
	, 	event_pro 	= 'PMI__Button__Event__SeriesExportJpg' $
	,	pro_set_value 	= 'PMI__Button__Control__SeriesExportJpg' $
	)
end