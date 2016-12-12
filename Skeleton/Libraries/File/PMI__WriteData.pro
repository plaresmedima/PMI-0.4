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


pro PMI__WriteData, data, file, i, error=error


	append = n_params() eq 2



	;OPEN FILE AND EXIT IF PROBLEM OPENING (ERROR = -1)



	openu, 1, file, error=err, append=append

	if err ne 0 then begin
		error = -1
		close, 1
		return
	endif



	;WRITE DATA (ERROR = 1)



	error = 1

	if not append then point_lun, 1, i*PMI__nbytes(size(data,/type))

	writeu, 1, data
	close, 1
end