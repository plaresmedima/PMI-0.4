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


PRO FILE_MANAGER::DEFAULT_PATH, path
	self.default_path=path
END
FUNCTION FILE_MANAGER::DEFAULT_PATH
	return, self.default_path
END
FUNCTION FILE_MANAGER::GET_FILE, file, title=title, filter=filter, filename=filename, new=new, path=path0

	if n_elements(path0) 	eq 0 then path0		= self.default_path
	if n_elements(title) 	eq 0 then title		='Open file'
	if n_elements(filter) 	eq 0 then filter	='.*'
	if n_elements(filename) eq 0 then filename 	='file'
	file = dialog_pickfile($
		get_path 	= path, $
		path 		= path0, $
		title 		= title, $
		file 		= filename, $
		filter 		= '*' + filter)
	if file eq '' then return, 0

	self.default_path = path

	suffix = strmid(file,strlen(file)-strlen(filter),strlen(filter))
	if (suffix ne filter) and (filter ne '.*') then file = file + filter

	if not keyword_set(new) then return, 1

	files = findfile(fpath(file) + fname(file)+ '*' + filter,count=n)
	if n gt 0 then begin
		parts = str_sep(file,filter)
		n = strcompress(n+1,/remove_all)
		file  = parts[0] + '(' + n + ')' + filter
	endif

	return, 1
END

FUNCTION FILE_MANAGER::GET_DIR, path, title=title, files=files, cnt=cnt

	if n_elements(title) 	eq 0 then title		='Open folder'

	path = dialog_pickfile(/directory, path=self.default_path, title=title)
	if path eq '' then return, 0

	self.default_path = path

	if arg_present(files) then files=FindAllFiles(path,count=cnt)

	return, 1
END

FUNCTION FILE_MANAGER::CLEANUP
END

FUNCTION FILE_MANAGER::INIT

	if !version.os eq 'Win32' then self.slash = '\'
	if !version.os eq 'linux' then self.slash = '/'
	s = str_sep(!path,';')
	self.default_path = s[n_elements(s)-1] + self.slash
END

PRO FILE_MANAGER__DEFINE

	struct = {FILE_MANAGER		,$
		slash:				''	,$	;'\' for windows and '/' for linux
		default_path: 		''	 }	;default path for opening, importing, exporting, ...

END