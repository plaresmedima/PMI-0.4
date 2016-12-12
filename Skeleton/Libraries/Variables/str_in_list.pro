;modifies 'str' to be part of 'list'
;when 'str' is already 'n' times in 'list'
;a string '(n+1)' is appended,
;except when it already has the form 'name(n)'
;in which case it is changed to 'name(n+1)'
;when a suffix is given as input,
;the modifications are made before the suffix


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


function str_in_list, list, strIn, suffix=suffix, FOLD_CASE=FOLD_CASE

	str = strIn

	nlist = n_elements(list)

	;REMOVE SUFFIX, IF GIVEN
	if n_elements(suffix) ne 0 then begin
		nsuffix = strlen(suffix)
		str = strmid(str,0,strlen(str)-nsuffix)
		for i=0L,nlist-1 do list[i] = strmid(list[i],0,strlen(list[i])-nsuffix)
	endif

	;REMOVE '(n0)' IF PRESENT AT THE END
	on_ioerror, break
	sep = str_sep(str,'(')
	nsep = n_elements(sep)
	cut = 0
	if nsep gt 1 then begin
		last = sep[nsep-1]
		char = strmid(last,strlen(last)-1,1)
		if char ne ')' then goto, break
		ns0 = strmid(last,0,strlen(last)-1)
		if ns0 eq '' then goto, break
		n0 = long(ns0)
	 	str = strmid(str,0,strlen(str)-strlen(ns0)-2)
 		cut = 1
 	endif
	break:

	;COUNT OCCURENCES: n
	nstr = strlen(str)
	n = -1
	on_ioerror, kdef
	for i=0L,nlist-1 do begin
		tmp = strmid(list[i],0,nstr)
		if tmp eq str then begin
			k = -1 & valid = 0
			nstr_i = strlen(list[i])
			if nstr_i gt nstr then begin
				left = strmid(list[i],nstr,1)
				right = strmid(list[i],nstr_i-1,1)
				if (left eq '(') and (right eq ')') then begin
					nr = strmid(list[i],nstr+1,nstr_i-nstr-2)
					k = long(nr)
					valid = 1
					kdef:if not valid then k=-1
				endif
			endif else k=0
			if k gt n then n=k
		endif
	endfor

	;APPEND '(n)'
	if n ne -1 then begin
		n_add = n+1
		if cut then begin
			if n0 gt n_add then n_add = n0
		endif
		str = str + '(' + strcompress(n_add,/remove_all) + ')'
	endif

	;APPEND SUFFIX, IF GIVEN
	if n_elements(suffix) ne 0 then str = str + suffix

	return, str
end