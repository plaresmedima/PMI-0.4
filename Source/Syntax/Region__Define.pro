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


PRO REGION::EXTRUDE, Path

	for i=0L,self->d(2)-1 do begin

		Bin = Self->Read(Path,i,0)
		for j=1L,self->d(3)-1 do Bin = Bin or Self->Read(Path,i,j)
		if total(Bin) gt 0 then for j=0L,self->d(3)-1 do Self->Write, Path, bin, i, j
	endfor
END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		READ		 	    	  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



FUNCTION REGION::SLICEPOS, z, t, ok=ok

	Rz=self->z() & iz=where(Rz eq z) & iz=iz[0]
	Rt=self->t() & it=where(Rt eq t) & it=it[0]

	ok = (iz ne -1) and (it ne -1)

	return, [iz,it]
END

FUNCTION REGION::WHERE, path, i,j, pos=p, z=z, t=t, n=n, Inverse=Inverse

	case n_params() of
		1:data = self->read(path, pos=p, z=z, t=t)
		2:data = self->read(path,i)
		3:data = self->read(path,i,j)
	endcase

	if size(data,/n_dimensions) eq 0 then begin
		n = 0L
		return, [-1]
	endif

	if keyword_set(inverse) then return, where(data eq 0,n)
	return, where(data eq 1,n)

END





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		CLEANUP, INIT and DEFINE  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



PRO REGION::CLEANUP
END

FUNCTION REGION::INIT, File, name=name

	if n_elements(name) eq 0 then name = 'REGION'

	self.file = file
	self.Name = name
	self.type = 1
	self->clr

	return, 1
END

PRO REGION__DEFINE

	struct = {REGION					$
	,	INHERITS Data					$
	}

END