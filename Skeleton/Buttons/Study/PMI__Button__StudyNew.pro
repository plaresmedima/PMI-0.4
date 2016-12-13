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


pro PMI__Button__Event__StudyNew, ev

	PMI__info, ev.top, state=State, Path=Path, Viewer=Viewer
	file = Path + 'STUDY.pmi'
	files = State -> files(n)
	if n gt 0 then file = str_in_list(files,file,suffix='.pmi')
	if not State -> get_file(file,title='Save study as', filter='.pmi',/new) then return
	Stdy = obj_new('STDY_STATE',file)
	Stdy -> SaveStdy
	State -> Insert, Stdy
	PMI__Button__RecentStudy__Build, ev.top, /update
	if Viewer ne 'PMI__DISPLAY__2DVIEW' then PMI__Control, ev.top, Viewer = 'PMI__DISPLAY__2DVIEW'
	PMI__control, ev.top, /refresh
end

pro PMI__Button__Control__StudyNew, id, v
end

function PMI__Button__StudyNew, parent

	PMI__DISPLAY__2DVIEW__Define

	return, widget_button(parent $
	, 	value='New'	 $
	, 	event_pro= 'PMI__Button__Event__StudyNew' $
	, 	pro_set_value = 'PMI__Button__Control__StudyNew' $
	, 	ACCELERATOR='Ctrl+N' $
	)
end