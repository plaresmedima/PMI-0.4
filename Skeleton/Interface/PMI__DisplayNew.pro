;
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


FUNCTION PMI__DisplayNew, id, Viewer, _Extra=e

	PMI__Info, id, Display=Display
	widget_control, id, update=0
	gpmi = widget_info(/geometry, id)
	WinId = widget_info(widget_info(id,/child),/sibling)
	Display -> GET, CursorPos = P
	obj_destroy, Display
	Display = obj_new(Viewer,WinID,P,xsize=gpmi.xsize,ysize=gpmi.ysize,_Extra=e)
	PMI__Control, id, /MenuRefresh
	widget_control, id, update=1
	return, Display
END