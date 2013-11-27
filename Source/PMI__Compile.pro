;    Creates a PMI runtime version
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




pro PMI__Compile

;	path = str_sep(!path,';')
;	path = path[n_elements(path)-1]

	path = dialog_pickfile(/directory, $
		path 	= 'C:\PMI\PMI 0.4\', $
		title	= 'Save pmi.sav in the directory..' )
	if path eq '' then return
	file = path + 'pmi.sav'


	;COMPILE


	resolve_routine, 'pmi'
	resolve_all



	PMI__OBJECTS



	;iTools

	ITRESOLVE


	save, /routines, filename=file,/compress
	print, 'saved to ', file
end