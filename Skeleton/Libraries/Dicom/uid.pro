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


function UID, type

	if n_elements(type) eq 0 then type = 'OTHER'

	case type of
		'OTHER'		:str='000'
		'STUDY'		:str='001'
		'SERIES'	:str='002'
		'SOP'		:str='003'
		'MSSOP'		:str='004'
	endcase

    root = '1.3.12.2' ;SIEMENS ROOT

	caldat,systime(/julian),month,day,year,hour,minute,second

	suffix = strcompress(year,/remove_all) $
		+ '.' + strcompress(month,/remove_all) $
		+ '.' + strcompress(day,/remove_all) $
		+ '.' + strcompress(hour,/remove_all) $
		+ '.' + strcompress(minute,/remove_all) $
		+ '.' + strcompress(floor(second),/remove_all)

    UID = root + '.' + suffix + '.' + str

    l = strlen(UID)
    if floor(l/2.0) ne l/2.0 then UID = UID + '0'

    return, UID
end