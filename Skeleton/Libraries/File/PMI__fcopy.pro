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


pro PMI__fcopy, from, to, n=n

	memsize = 64L*10L^6

	openr, unitfrom, from, error=errfrom, /get_lun
	openw, unitto, to, error=errto, /get_lun

	if (errfrom ne 0) or (errto ne 0) then goto, return

	s = fstat(unitfrom)
	n = s.size

	if n le memsize then begin
		nlumps = 1L
		nremainder = 0L
		lump = bytarr(n)
	endif else begin
		nlumps = n/memsize
		nremainder = n - nlumps*memsize
		lump = bytarr(memsize)
	endelse

	p=0L
	for k=0L,nlumps-1 do begin
		point_lun, unitfrom, p
		point_lun, unitto, p
		readu, unitfrom, lump
		writeu, unitto, lump
		p=p+memsize
	endfor

	if nremainder gt 0 then begin
		lump = bytarr(nremainder)
		point_lun, unitfrom, p
		point_lun, unitto, p
		readu, unitfrom, lump
		writeu, unitto, lump
	endif

	return:

	close, unitfrom
	free_lun, unitfrom
	close, unitto
	free_lun, unitto
end