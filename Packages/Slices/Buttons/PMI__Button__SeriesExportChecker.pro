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
pro PMI__Button__Event__SeriesExportChecker, ev

	PMI__Info, ev.top, Series=Series1, Status=Status, Stdy=Stdy

	in = PMI__Form(ev.top, Title='Checker-board setup', [$
	ptr_new({Type:'VALUE'	,Tag:'name',Label:'Save checkerboard as', Value:Series1->Name()}), $
	ptr_new({Type:'DROPLIST',Tag:'img' ,Label:'Second image', Value:Stdy->names(0,Dim=Series1->d(),Ind=ind)}), $
	ptr_new({Type:'VALUE'	,Tag:'pix' ,Label:'Width of squares (pixels)', Value:20L})])
	if in.cancel then return

	Series2 = Stdy -> Obj(0,ind[in.img])
	d = Series1 -> d ()

	Path = Stdy->ExportPath()

	PMI__Control, ev.top, Viewer = 'PMI__Display__Image', Display=Display

	for j=0L,d[3]-1 do begin
	for i=0L,d[2]-1 do begin

		PMI__Message, status, 'Exporting ', (j*d[2]+i)/(d[2]*d[3]-1.0)

		file = Path + in.name + '_(' + strnr(i,3) + ',' + strnr(j,3) + ')_Fusion.tif'
		im1 = Series1 -> image(Stdy->DataPath(),i,j,/bits24)
		im2 = Series2 -> image(Stdy->DataPath(),i,j,/bits24)
		im = PMI__Checkerboard(im1,im2,in.pix)
		write_tiff, file, reverse(im,3)

		Display -> Set, Image=im, /Refresh
	endfor
	endfor

	PMI__Message, status
end

pro PMI__Button__Control__SeriesExportChecker, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	sensitive = 0
	if obj_valid(Stdy) then sensitive = Stdy->n(0) ge 2
	widget_control, id, sensitive = sensitive
end
function PMI__Button__SeriesExportChecker, parent, value=value, separator= separator

	PMI__Display__Image__Define

	if n_elements(value) eq 0 then value = 'Export Checkerboard'

  	return, widget_button(parent, $
  	 	separator = separator, $
  	 	value = value, $
	 	event_pro = 'PMI__Button__Event__SeriesExportChecker', $
		pro_set_value = 'PMI__Button__Control__SeriesExportChecker')

end