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


pro PMI__Button__RecentStudy__Update, filename

	cd, current=current_dir

	if (file_test(current_dir+'\recent.dat') eq 1) then begin
	 	;Write Path to recent path list:
		OPENU, unit, current_dir+'\recent.dat', /get_lun
		rows = file_lines(current_dir+'\recent.dat')
		line = ''
		text = strarr(rows)

		;load file to strarray
	   	for i=0l, rows -1 do begin
	            readf, unit, line
	            text(i) = line
	    endfor

		;check if already in list

		for i=0l, rows-1 do begin
			if strcmp(text(i),filename) then begin
				for j = 1, i do begin
					text(-j+1+i) = text(-j+i)
				endfor
				text(0) = filename
				break
			endif
			if i eq rows -1 then begin
				if rows eq 5 then begin
				for j = 0 , i-1 do begin
					text(-j+rows-1) = text(-j+rows-2)
				endfor
				text(0) = filename
				endif else begin
					text = [filename,text]
				endelse
			endif
		endfor
		free_lun, unit

	endif else begin

		openw, unit, current_dir+'\recent.dat',/get_lun
		text = strarr(1)
   		text(0) = filename
   		free_lun, unit
	endelse

	openw, unit, current_dir+'\recent.dat', /get_lun
		   minim = min([n_elements(text)-1,4])
    	   for i=0l, minim do printf, unit, text(i)
    free_lun, unit


end

function PMI__Button__RecentStudy__Load, rows

	cd, current = current_dir
	rows = 0L

	;Load recently openend studies
	if (file_test(current_dir+'\recent.dat') eq 1) then begin
	 	;Write Path to recent path list:
		OPENU, unit,current_dir+'\recent.dat', /get_lun
		line = ''
		rows = file_lines(current_dir+'\recent.dat')
	   	recent = strarr(rows)
		;load file to strarray
	   	for i=0l, rows-1 do begin
	            readf, unit, line
	            recent(i) = line
	    endfor
		free_lun, unit
		return, recent
	endif
end

pro PMI__Button__Event__OpenRecentStudy, ev
	widget_control,  ev.id, get_uval=recent
	if (file_test(recent) eq 1) then begin
		PMI__Button__StudyOpen__Open, ev.top, recent
	endif else begin
		msg = ['Open Study failed. Selected study has been deleted or moved']
       ok = dialog_message(msg,/information)
	   ;; delete corresponding button

	   widget_control, ev.id, /destroy
       return
    end
end

pro PMI__Button__Control__RecentStudyButton, id, v
	;Buttons with recent studies are sensitive iff they exist
end

pro PMI__Button__Control__RecentStudyMenu, id, v
	;called only if the recent study menu has no children
	;ie. when the list of recent studies is empty
	;in that case the menu is insensitive
	widget_control, id, sensitive = 0
end

pro PMI__Button__RecentStudy__Build, top, update=update

	if keyword_set(update) then begin
		PMI__info, top, Stdy=stdy
		PMI__Button__RecentStudy__Update, stdy->file()
	endif

	menu = widget_info(top,find_by_uname='Recent')

	;Destroy all buttons with recent studies
	n = widget_info(menu,/n_children)
	id = widget_info(menu,/all_children)
	for i=0L,n-1 do widget_control, id[i], /destroy

	;Load list of recent studies and create one button for each
	recent = PMI__Button__RecentStudy__Load(n)
	for i=0L,n-1 do begin
		if (file_test(recent(i)) eq 1) then begin
			Sid = widget_button(menu, value = recent(i)$
							  , event_pro = 'PMI__Button__Event__OpenRecentStudy'$
							  ,	pro_set_value = 'PMI__Button__Control__RecentStudyButton'$
							  , uval = recent(i) $
							  , uname = strcompress('recent'+string(i)) $
							  , ACCELERATOR = string('Ctrl+',i+1))
		endif
	endfor

end

function PMI__Button__RecentStudy, parent

	id 	= widget_button(parent,/menu $
	,	value = 'Recent' $
	,	uname = 'Recent' $
	,	pro_set_value = 'PMI__Button__Control__RecentStudyMenu' )

	PMI__Button__RecentStudy__Build, tlb(parent)

	return, id
end