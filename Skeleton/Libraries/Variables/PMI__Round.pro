;Returns the number x, rounded off to n digits after the comma (n=0,1,2,..)
;if the keyword string is set, the rounded value is returned as a string, with zeros removed


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

function PMI__Round, x, n, string=string

	if n_params() eq 1 then return, round(x)

	if finite(x) eq 0 then xr=round(0D) else xr = round(x*10D^n,/L64)/10D^n

	if not keyword_set(string) then return, xr

	s = strcompress(xr,/remove_all)
	s = strsplit(s,'.',/extract)
	if n eq 0 then return, s[0]
	if n_elements(s) eq 1 then return, s[0]
	s[1] = strmid(s[1],0,n)
	return, s[0] + '.' + s[1]

end
