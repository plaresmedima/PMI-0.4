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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		COLORS	      			  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




FUNCTION SERIES::BYTE_VALUE, r
	v 	= self -> value(path, r)
	v 	= bytscl(v,min=self.trim[0],max=self.trim[1])
	return, v
END
FUNCTION SERIES::IMAGE, path, i, j, clrstrip=clrstrip, bits24=bits24
	im 	= self -> read(path,i,j)
	im = bytscl(im,min=self.trim[0],max=self.trim[1])
	if keyword_set(clrstrip) then im = addstrip(im,max=255)
	if keyword_set(bits24) then im = Image24Bit(im, self->Clr(/R), self->Clr(/G), self->Clr(/B))
	return, im
END
PRO SERIES::TRIM, x, i
	if n_params() eq 0 then begin
		self.trim = [0E,100E]
		return
	endif
	if n_params() eq 1 then self.trim=x else self.trim[i]=x
END
FUNCTION SERIES::TRIM, i
	if n_params() eq 0 then return, self.trim
	return, self.trim[i]
END





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		READ AND WRITE 	    	  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






FUNCTION SERIES::VALUE, path, p
	d 	= self -> d()
	np 	= n_elements(p)
	v 	= fltarr(np)
	for i=0L,np-1 do begin
		r = reform_ind(d,ind=p[i])
		im = self->Read(path,r[2],r[3])
		v[i] = im[r[0],r[1]]
	endfor
	return, v
END
FUNCTION SERIES::CURVE, path, p
	d = self -> d()
	np = n_elements(p)
	c = fltarr(np,d[3])
	for i=0L,np-1 do begin
		r = reform_ind(d[0:2],ind=p[i])
		for k=0L,d[3]-1 do begin
			im = self->Read(path,r[2],k)
			c[i,k] = im[r[0],r[1]]
		endfor
	endfor
	for k=0L,d[3]-1 do c[0,k] = total(c[*,k])/np
	return, reform(c[0,*])
END
FUNCTION SERIES::PIXELCURVE, path, r
	d = self -> d()
	c = fltarr(d[3])
	v = 0E
	openr, unit, path+self.file, /get_lun
	for k=0L,d[3]-1 do begin
		i = self -> SliceIndx(k*d[2]+r[2])
		if i ne -1 then begin
			point_lun, unit, 4*(i*d[0]*d[1]+r[1]*d[0]+r[0])
			readu, unit, v
			c[k] = v
		endif
	endfor
	close, unit
	free_lun, unit
	return, c
END





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		CLEANUP, INIT and DEFINE  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




PRO SERIES::CLEANUP
END

FUNCTION SERIES::INIT, File, name=name

	if n_elements(name) eq 0 then name = 'SERIES'

	self.file = file
	self.Name = name
	self.trim = [0E,100E]
	self.type = 4
	self->clr

	return, 1
END

PRO SERIES__DEFINE

	struct = {SERIES				$
	,	INHERITS Data				$
	,	trim:			fltarr(2)	}
END