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


pro PMI__Button__Event__StudySaveAs, ev

	PMI__info, ev.top, Stdy=Stdy, State=State
	if not State->get_file(file,title='Save study as',filter='.pmi',/new) then return
 	oldpath 	= Stdy->DataPath()
 	oldfile		= Stdy->file()
 	Stdy->file, file
	newpath 	= Stdy->DataPath()
	for k=0,1 do begin
		for i=0L,Stdy->n(k)-1 do begin
			DataFile = (Stdy->Obj(k,i))->File()
			PMI__fcopy, oldpath+DataFile, newpath+DataFile
		endfor
	endfor
	Stdy -> SaveStdy
	Stdy -> UpdateDir, oldfile
	PMI__Button__RecentStudy__Build, ev.top, /update
	PMI__control, ev.top, /refresh
end

pro PMI__Button__Control__StudySaveAs, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	widget_control, id, sensitive=obj_valid(Stdy)
end

function PMI__Button__StudySaveAs, parent

	return, widget_button(parent $
	, 	value='Save as' $
	, 	event_pro= 'PMI__Button__Event__StudySaveAs' $
	, 	pro_set_value = 'PMI__Button__Control__StudySaveAs' $
	,	ACCELERATOR='Ctrl+A' $
	)

end