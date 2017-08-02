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



pro PMI__Button__Event__RegionExportBmp, ev

	;GET FILENAME

	PMI__info, ev.top, State=s, Region=data, Status=Status, Stdy=Stdy

	if not s -> get_file(file, title= 'Save images as', filter= '.bmp') then return
	file = strmid(file,0,strlen(file)-strlen('.bmp'))

	;SAVE IMAGES

	d = data -> d ()

	for j=0L,d[3]-1 do begin
	for i=0L,d[2]-1 do begin

		PMI__Message, status, 'Exporting ', (j*d[2]+i)/(d[2]*d[3]-1.0)

		file_ij = file + '(' + strnr(i,3) + ',' + strnr(j,3) + ').bmp'
		im 		= data -> read(Stdy->DataPath(),i,j)
		write_bmp, file_ij, 255*im
	endfor
	endfor

	PMI__Message, status
end

pro PMI__Button__Control__RegionExportBmp, id, v

	PMI__Info, tlb(id), Region=Region
	widget_control, id, sensitive = obj_valid(Region)
end
function PMI__Button__RegionExportBmp, parent, separator	= separator

  	return, widget_button(parent $
  	, 	separator	= separator	$
  	, 	value		= 'BMP' $
	, 	event_pro 	= 'PMI__Button__Event__RegionExportBmp'	$
	,	pro_set_value = 'PMI__Button__Control__RegionExportBmp' $
	)
end