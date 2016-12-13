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


pro PMI__Menu__Region, parent

	id 	= widget_button(parent, value='Region', /menu)

	Sid = PMI__Button__RegionDelete(id)
	Sid = PMI__Button__RegionDuplicate(id)
;	Sid = PMI__Button__RegionRename(id)

	Sid = widget_button(id,/menu,value='Import',/separator)

	SSid = PMI__Button__RegionImportRaw(Sid)

	Sid = widget_button(id,/menu,value='Export')

	SSid = PMI__Button__RegionExportBmp(Sid)
	SSid = PMI__Button__RegionExportRaw(Sid)

	Sid = PMI__Button__RegionExtrude(id,/separator)
	Sid = PMI__Button__RegionCombine(id)
	Sid = PMI__Button__RegionBinary(id)

;	Sid = PMI__Button__RegionByRange(id, value='Threshold segmentation (Numerical)', /separator)
	Sid = PMI__Button__RegionSelectByRangeInteractive(id, value='Threshold', /separator)
	Sid = PMI__Button__RegionShrink(id)


end