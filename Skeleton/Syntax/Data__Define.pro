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


PRO DATA::CLR, R, G, B
	if n_params() eq 0 then begin
		R = bindgen(256)
		G=R & B=R
	endif
	if n_elements(R) eq 3L*256 then begin
		self.ClrScl= R
		return
	endif
	self.ClrScl[0:255] 		= R
	self.ClrScl[256:511] 	= G
	self.ClrScl[512:767] 	= B
END
FUNCTION DATA::CLR, R=R, G=G, B=B, SAT=sat

	if keyword_set(R) then return, self.ClrScl[0:255]
	if keyword_set(G) then return, self.ClrScl[256:511]
	if keyword_set(B) then return, self.ClrScl[512:767]
	if n_elements(sat) ne 0 then begin
		C = self.ClrScl
		case sat of
			'R':C[0:255]	=255
			'G':C[256:511]	=255
			'B':C[512:767]	=255
		endcase
		return, C
	endif
	return, self.ClrScl
END




FUNCTION DATA::SLICEINDX, k

	if not ptr_valid(self.indx) then return, -1
	ind = where(k eq *self.indx, cnt)
	if cnt eq 0 then return, -1
	return, ind[0]
END
PRO DATA::SLICEINDX, k

	if not ptr_valid(self.indx) then begin
		self.indx = ptr_new([k])
	endif else begin
		ks = self->sliceindx(k)
		if ks eq -1 then begin
			*self.indx = [*self.indx,k]
		endif
	endelse
END




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		READ AND WRITE	    	  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


FUNCTION DATA::READPIXEL, path, k, i
	a = make_array(1,type=self.type)
	d = self -> d()
	ks = self -> SliceIndx(k)
	if ks ge 0 then a = PMI__ReadData(path+self.file,self.type,position=long64(ks)*d[0]*d[1]+i,length=1)
	return, a[0]
END

FUNCTION DATA::READSLICE, path, k

	d = self -> d()
	ks = self -> SliceIndx(k)
	if ks eq -1 then return, make_array(d[0],d[1],type=self.type)
	im = PMI__ReadData(path+self.file,self.type,position=long64(ks)*d[0]*d[1],length=d[0]*d[1],error=err)
	if err ne 1 then return, make_array(d[0],d[1],type=self.type)
	return, reform(im,d[0:1],/overwrite)
END
PRO DATA::WRITESLICE, path, data, k

	ks = self -> SliceIndx(k)
	if ks eq -1 then begin
		PMI__WriteData, data, path+self.file
	endif else begin
		d=self->d()
		PMI__WriteData, data, path+self.file, long64(ks)*d[0]*d[1]
	endelse
	self -> SliceIndx, k
END


PRO DATA::WRITE, path, data, i, j

	d = self -> d()

	self -> SetValue, '1911'x, '0003'x, 1
	case n_params() of
	2:	begin
			data = reform(data,d[0]*d[1],d[2]*d[3],/overwrite)
			for k=0L,d[2]*d[3]-1 do self -> WriteSlice, path, data[*,k], k
		end
	3:	self -> WriteSlice, path, data, i
	4:	begin
			if i lt 0 then begin
				data = reform(data,d[0]*d[1],d[2],/overwrite)
				for k=0L,d[2]-1 do self -> WriteSlice, path, data[*,k], k + j*d[2]
				return
			endif
			if j lt 0 then begin
				data = reform(data,d[0]*d[1],d[3],/overwrite)
				for k=0L,d[3]-1 do self -> WriteSlice, path, data[*,k], i + k*d[2]
				return
			endif
			self -> WriteSlice, path, data, i + j*d[2]
		end
	endcase
END



FUNCTION DATA::READ, path, i, j, pos=p, z=z, t=t

	if n_elements(p) ne 0 then return, self->read(path,z=p[0],t=p[1])

	if 	(n_elements(z) ne 0) $
	and (n_elements(t) ne 0) then begin
		i = where(float(z) eq float(self->z()), n)
		if n eq 0 then return, 0B
		j = where(float(t) eq float(self->t()), n)
		if n eq 0 then return, 0B
		return, self->read(path,i[0],j[0])
	endif

	if n_elements(z) ne 0 then begin
		i = where(float(z) eq float(self->z()), n)
		if n eq 0 then return, 0B
		return, self->read(path,i[0],-1)
	endif

	if n_elements(t) ne 0 then begin
		j = where(float(t) eq float(self->t()), n)
		if n eq 0 then return, 0B
		return, self->read(path,-1,j[0])
	endif

	d = self -> d()
	case n_params() of
	1:	begin
			data = make_array(d[0]*d[1],d[2]*d[3],type=self.type)
			for k=0L,d[2]*d[3]-1 do if self->SliceIndx(k) ne -1 then data[*,k] = self->ReadSlice(path,k)
			data = reform(data,d,/overwrite)
		end
	2:	data = self->ReadSlice(path,i)
	3:	begin
		if i lt 0 then begin
			data = make_array(d[0],d[1],d[2],type=self.type)
			for k=0L,d[2]-1 do begin
				if self->SliceIndx(k+j*d[2]) ne -1 $
				then data[*,*,k] = self->ReadSlice(path,k+j*d[2])
			endfor
			return, data
		endif
		if j lt 0 then begin
			data = make_array(d[0],d[1],d[3],type=self.type)
			for k=0L,d[3]-1 do begin
				if self->SliceIndx(i+k*d[2]) ne -1 $
				then data[*,*,k] = self->ReadSlice(path,i+k*d[2])
			endfor
			return, data
		endif
		data = self->ReadSlice(path,i+j*d[2])
		end
	endcase
	return, data
END






PRO DATA::OPEN, file, unit, ok, n=n
	get_lun, unit
	openr, unit, file, error=err
	ok = err eq 0
	if not ok then begin
		msg = 	[	'The following data: '						$
				,	'FILE: ' + file						$
				,	'NAME: ' + self.name						$
				,	'have been deleted by another application'	]
		answer = dialog_message(msg,/information)
		close	, unit
		free_lun, unit
	endif
	f = fstat(unit)
	n = f.size/PMI__nbytes(self.type)
END



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		FILES	    	  			;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




PRO DATA::MOVETO, oldpath, newpath
	PMI__fcopy, oldpath+self.file, newpath+self.file
	file_delete, oldpath+self.file
END
PRO DATA::FILE, file
	self.file = file
END
FUNCTION DATA::FILE
	return, self.file
END
PRO DATA::NAME, x
	self.name = x
END
FUNCTION DATA::NAME
	return, self.name
END
PRO DATA::TYPE, x
	self.type = x
END
FUNCTION DATA::TYPE
	return, self.type
END






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		CLEANUP, INIT and DEFINE  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





PRO DATA::CLEANUP
END

FUNCTION DATA::INIT

	self -> clr
	return, 1
END

PRO DATA__DEFINE

	struct = {DATA			$
	,	INHERITS Domain		$
	,	INHERITS Header		$
	,	file: ''			$
	,	type: 0B			$
	,	indx: ptr_new()		$
	,	name: ''			$
	,	ClrScl:	bytarr(768)	$
	}

END