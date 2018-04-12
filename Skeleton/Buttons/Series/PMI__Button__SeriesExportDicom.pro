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


pro PMI__Button__Event__SeriesExportDicom, ev

	PMI__info, ev.top, State=s, Series=data, Status=Status, Stdy=Stdy

	if not s -> get_file(file, title = 'Save data as', filter='.dcm') then return
	file = strmid(file,0,strlen(file)-strlen('.dcm'))


	dcm = LMU__DicomTemplate('PMI output') ;Fill in default values for the most essential dicom header elements
;	dcm -> CopyHeader, Data

	d = data -> d()

	for k=0L,d[2]*d[3]-1 do begin

		PMI__Message, status, 'Exporting DICOM series', k/(d[2]*d[3]-1E)

		r = reform_ind(d[2:3],ind=k)

		dcm -> SetImage, data->Read(Stdy->DataPath(), r[0],r[1]) ;set pixel data, nr of row, columns and rescale slope+intercept
		dcm -> SetValue,'0020'x,'0013'x,uint(k)		;Image number
		dcm -> SetValue,'0020'x,'1041'x,data->z(r[0])	;Slice Location
		dcm -> SetValue,'0008'x,'1032'x,data->t(r[1])	;Acquisition time

		dcm -> Write, file + '(' + strnr(r[0],3) + ',' + strnr(r[1],3) + ').dcm'
	endfor

	obj_destroy, dcm

	PMI__Message, status
end

pro PMI__Button__Control__SeriesExportDicom, id, v

	PMI__Info, tlb(id), Series=Series
	widget_control, id, sensitive = obj_valid(Series)
end
function PMI__Button__SeriesExportDicom, parent, separator = separator, value=value

	if n_elements(value) eq 0 then value = 'DICOM'

  	return, widget_button(parent	$
  	, 	separator = separator $
  	, 	value = value $
	, 	event_pro = 'PMI__Button__Event__SeriesExportDicom' $
	,	pro_set_value 	= 'PMI__Button__Control__SeriesExportDicom' $
	)
end