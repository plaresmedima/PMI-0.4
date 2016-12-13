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


function PMI__ReadData, file, type, position=p, length=l, error=error

	;OPEN FILE AND EXIT IF PROBLEM OPENING (ERROR = -1)




	openr, 1, file, error=err

	if err ne 0 then begin
		error = -1
		close, 1
		return, 0B
	endif




	;EXIT IF INSUFFICIENT DATA IN THE FILE (ERROR = 0)



	s = fstat(1)
	n = s.size/PMI__nbytes(type)

	if n_elements(p) eq 0 then p=0L
	if n_elements(l) eq 0 then l=n-p

	if (p+l gt n) or (l le 0) then begin
		error = 0
		close, 1
		return, 0B
	endif



	;READ DATA AND CLOSE FILE (ERROR = 1)



	error = 1

	data = make_array(l,type=type,/nozero)

	point_lun	, 1, p*PMI__nbytes(type)
	readu		, 1, data
	close		, 1

	return, data
end