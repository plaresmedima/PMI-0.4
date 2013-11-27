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


pro PMI__Menu__Series, parent

    id = widget_button(parent, value= 'Series',/menu)

    Sid = PMI__Button__SeriesDelete(id)
    Sid = PMI__Button__SeriesDuplicate(id)

    Sid = widget_button(id,value= 'Import',/menu,/separator)

    SSid = PMI__Button__SeriesImportDicom(Sid, value='DICOM')
    SSid = PMI__Button__SeriesImportRaw(Sid)

    Sid = widget_button(id,/menu,value='Export')

;    SSid = PMI__Button__SeriesExportDicom(Sid)
    SSid = PMI__Button__SeriesExportTif32(Sid)
    SSid = PMI__Button__SeriesExportTif24(Sid)
    SSid = PMI__Button__SeriesExportBmp(Sid)
    SSid = PMI__Button__SeriesExportJpg(Sid)
    SSid = PMI__Button__SeriesExportGif(Sid,/separator)
    SSid = PMI__Button__SeriesExportCsv(Sid, value='CSV',/separator)

	Sid = widget_button(id,/menu,value='Edit',/separator)

    SSid = PMI__Button__SeriesEditTimeResolution(Sid, value='Time Resolution')
    SSid = PMI__Button__SeriesEditCoordinates(Sid, value='All coordinates')

	Sid = PMI__Button__SeriesMerge(id,/separator)
	Sid = PMI__Button__SeriesMergeSlices(id)
	Sid = PMI__Button__SeriesExtract(id)

	Sid = PMI__Button__SeriesTransform(id,value='Transform',/separator)
	Sid = PMI__Button__SeriesBinary(id, value='Combine')

end
