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


function FindAllFiles, path, count=n
	; findfile is deprecated since IDL 5.5
	; instead, file_search is recommended by RSI
	; by the way, this makes the whole routinde findallfiles obsolete
	; we keep it as a wrapper around file_search
	PathFiles = file_search(path,'*', /TEST_REGULAR, count=n)
	return, PathFiles

	; this is the old routine
;	PathFiles = findfile(path + '*', count=n)
;	PathFiles = PathFiles[2:*]
;
;	LastChar = strarr(n)
;	for i=0L,n-1 do LastChar[i] = strmid(PathFiles[i],strlen(PathFiles[i])-1,1)
;	dir = LastChar eq '\'
;
;
;	files = ['']
;	n = 0L
;
;	idir = where(dir eq 1, ndir)
;	for i=0L,ndir-1 do begin
;		fi = FindAllFiles(PathFiles[idir[i]],count=ni)
;		if ni gt 0 then begin
;			n = n + ni
;			files = [files,fi]
;		endif
;	endfor
;
;	ifil = where(dir eq 0, nfil)
;	if nfil gt 0 then begin
;		n = n + nfil
;		files = [files,PathFiles[ifil]]
;	endif
;
;	if n eq 0 then return, ''
;
;	return, files[1:*]
end
