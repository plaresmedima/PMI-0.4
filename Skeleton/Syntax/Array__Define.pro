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


FUNCTION ARRAY::GETVALUE
	if ptr_valid(self.ptr) then return, *self.ptr
	return, [obj_new()]
END
PRO ARRAY::SETVALUE, arr
	self.ptr = ptr_new(arr)
	self.i = 0
END
PRO ARRAY::INSERT, obj
	n = self->n()
	if n eq 0 then begin
		self.ptr = ptr_new([obj])
		self.i = 0
	endif else begin
		*self.ptr = [*self.ptr, obj]
		self.i = n
	endelse
END
PRO ARRAY::DELETE, i
	if n_elements(i) eq 0 then i = [self.i]
	obj_destroy, (*self.ptr)[i]
	j = where(obj_valid(*self.ptr),n);        ???
	if n eq 0 then begin
		ptr_free, self.ptr
		self.i = -1
	endif else begin
		(*self.ptr) = (*self.ptr)[j]
		self.i = 0
	endelse
END
PRO ARRAY::SEL, i
	self.i = i
END
FUNCTION ARRAY::SEL
	return, self.i
END
FUNCTION ARRAY::OBJ, i
	if not ptr_valid(self.ptr) then return, 0B
	if n_elements(i) eq 0 then i = self.i
	if i eq -1 then return, 0B
	return, (*self.ptr)[i]
END
FUNCTION ARRAY::N
	if not ptr_valid(self.ptr) then return, 0
	return, n_elements(*self.ptr)
END

PRO ARRAY::CLEANUP
	if not ptr_valid(self.ptr) then return
	obj_destroy, *self.ptr
	ptr_free, self.ptr
END

FUNCTION ARRAY::INIT, arr
	if n_params() eq 1 then begin
		self.ptr = ptr_new(arr)
	endif else self.i=-1
	return, 1
END

PRO ARRAY__DEFINE

	struct = {ARRAY	, ptr	:ptr_new() 	$
					, i		:0L			}
END