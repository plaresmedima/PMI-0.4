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


pro PMI__info, id 		$
, 	State	= State 	$
, 	Status	= Status 	$
,	Display = Display	$
, 	Stdy	= Stdy 		$
, 	Series	= Series 	$
, 	Region	= Region	$
,	Path 	= Path		$
,	Viewer = Viewer

	widget_control, id, get_uvalue = State

	if arg_present(Status) 	then Status = widget_info(id,find_by_uname='PMI__Module__MessageBoard')
	if arg_present(Stdy) 	then Stdy 	= State -> Obj()
	if arg_present(Series) 	then Series = State -> item(0)
	if arg_present(Region) 	then Region = State -> item(1)
	if arg_present(Path)	then Path = State -> Default_path()
	if arg_present(Display) then begin
		MenuBar = widget_info(id,/child)
		Window = widget_info(MenuBar,/Sibling)
		DisplayId = widget_info(Window,/Child)
		widget_control, DisplayId, get_uvalue = Display
	endif
	if arg_present(Viewer) then begin
		PMI__Info, id, Display=Display
		Viewer = Obj_Class(Display)
	endif
end