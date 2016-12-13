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


FUNCTION STATE::FILES, n

	n = self -> n()
	if n eq 0 then return, 0B
	files = strarr(n)
	for i=0L,n-1 do files[i] = (self->obj(i)) -> file()
	return, files
END
FUNCTION STATE::NAMES
	if self->n() eq 0 then return, 0B
	names = self->files(n)
	for i=0L,n-1 do names[i] = fname(names[i])
	return, names
END
FUNCTION STATE::ITEM, k, i

	if self->nitems(k) eq 0 then return, 0B
	stdy = self -> obj()
	return, stdy -> obj(k,i)
END
FUNCTION STATE::NITEMS, k

	if self->n() eq 0 then return, 0
	if self->sel() eq -1 then return, 0
	return,(self->obj())->n(k)
END
PRO STATE::CLEANUP

	n = self -> n()
	if n gt 0 then self -> delete, lindgen(n)
END
FUNCTION STATE::INIT

	ok = self -> file_manager::init()
	return, 1
END
PRO STATE__DEFINE

	struct = {STATE							$
	,	INHERITS file_manager				$
	,	INHERITS array						$  ;array of loaded studies
	}
END