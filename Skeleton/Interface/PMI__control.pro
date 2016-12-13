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



pro PMI__control, id $
,	Path = Path $
,	Refresh = refresh $
,	ButtonRefresh = ButtonRefresh $
,	MenuRefresh = MenuRefresh $
,	MenuSensitive=MenuSensitive $
,	Viewer = Viewer $	;OBSOLETE!!! REPLACE BY PMI__DISPLAYNEW()
,	Display = Display	;OBSOLETE

	if n_elements(Viewer) ne 0 then Display = PMI__DisplayNew(id,Viewer) ;OBSOLETE

	if n_elements(Path) ne 0 then begin
		PMI__Info, id, State=State
		State->Default_Path, Path
	endif

	if n_elements(ButtonRefresh) ne 0 then begin
		n = widget_info(ButtonRefresh,/n_children)
		if n gt 0 then begin
			widget_control, ButtonRefresh, /sensitive
			Child = widget_info(ButtonRefresh,/all_children)
			if n eq 1 then Child = [Child]
			ns=0
			for i=0L,n-1 do begin
				PMI__Control, id, ButtonRefresh = Child[i]
				ns = ns + widget_info(Child[i],/sensitive)
			endfor
			widget_control, ButtonRefresh, sensitive=ns
		endif else widget_control, ButtonRefresh, set_value='PMI__BUTTON__CONTROL'
	endif

	if keyword_set(MenuRefresh) then begin
		mbar = widget_info(id,/child)
		widget_control, id, update=0
		PMI__Control, id, ButtonRefresh=mbar
		widget_control, id, update=1
	endif

	if n_elements(MenuSensitive) ne 0 then begin

		Menu = widget_info(widget_info(id,/child),/all_children)
		for i=0L,n_elements(Menu)-1 do widget_control, Menu[i], sensitive=MenuSensitive
	endif

	if keyword_set(Refresh) then begin
		PMI__Info, id, State=State, Display=Display
		if State->n() gt 0 then Display->Set, /PMI__REFRESH $
		else Display = PMI__DisplayNew(id,'PMI__DISPLAY__WELCOME')
		PMI__Control, id, /MenuRefresh
	endif

end