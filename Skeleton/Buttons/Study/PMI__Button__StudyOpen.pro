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

pro PMI__Button__StudyOpen__Open, top, file

	PMI__info, top, state = State, Viewer=Viewer

	if State -> n() gt 0 then begin
		i = where(file eq State -> files(),cnt)
		if cnt gt 0 then begin
			msg = ['This Study is already open','Would you like to revert to the saved version?']
			if 'Yes' eq dialog_message(/question,msg) then begin
				stdy = State -> obj(i[0])
			   	stdy -> saved, 1
				State -> delete, i[0]
			endif else return
		endif
	endif
	Stdy = obj_new('STDY_STATE',file)
	State -> Insert, Stdy

	;Check if data have been removed

	for k=0,1 do begin
		nk = Stdy->n(k)
		if nk gt 0 then begin
			ind = bytarr(nk)
			for i=0L,nk-1 do begin
				Obj = Stdy->Obj(k,i)
				ind[i] = PMI__fopenr(Stdy->DataPath() + Obj->file())
			endfor
			ind = where(ind eq 0, cnt)
			if cnt gt 0 then Stdy -> Delete, k, ind
		endif
	endfor

	if Viewer eq 'PMI__DISPLAY__WELCOME' $
	then PMI__Control, top, Viewer = 'PMI__DISPLAY__2DVIEW' $
	else PMI__control, top, /refresh
end

pro PMI__Button__Event__StudyOpen, ev

	PMI__info, ev.top, state = State
	if not State -> get_file(file, title= 'Open study', filter= '.pmi') then return
	PMI__Button__StudyOpen__Open, ev.top, file
	PMI__info, ev.top, Stdy=stdy
	PMI__Button__RecentStudy__Build, ev.top, /update
	PMI__control, ev.top, /menurefresh
end


pro PMI__Button__Control__StudyOpen, id, v
end


function PMI__Button__StudyOpen, parent

	PMI__DISPLAY__2DVIEW__Define

	return, widget_button(parent $
	, 	value='Open'	$
	, 	event_pro= 'PMI__Button__Event__StudyOpen' $
	, 	pro_set_value = 'PMI__Button__Control__StudyOpen' $
	, 	ACCELERATOR='Ctrl+O' )
end