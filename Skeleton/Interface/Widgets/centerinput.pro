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


;Center Inputform in the PMI Display
PRO Centerinput,widget, pmi_pos

xCenter =	pmi_pos.XOFFSET+pmi_pos.XSIZE/2
yCenter = 	pmi_pos.YOFFSET+pmi_pos.YSIZE/2

geom = Widget_Info(widget, /Geometry)
xHalfSize = geom.Scr_XSize / 2
yHalfSize = geom.Scr_YSize / 2

Widget_Control, widget, XOffset = geom.xoffset+xCenter-xHalfSize, $
   YOffset = geom.Yoffset+yCenter-yHalfSize

END