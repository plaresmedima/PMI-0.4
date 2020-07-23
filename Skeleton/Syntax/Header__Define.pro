


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;                                           ;;;;;;;
;;;;;;             			GET               	 ;;;;;;;
;;;;;;                                           ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;









FUNCTION HEADER::LOCALIZERLINE, ImSlice, Localizer, LocSlice

	;Line = [a,b,c] are the coordinates in mm of a line ax+by+c=0,
	;Indicating the intersection with a Localizer.
	;[0,0,0] means that slice coordinates are not defined

	Default = [0E,0,0]

	;GENERIC

	Slices = self -> GET('5200'x,'9230'x, error=error)
	if error EQ 1B then return, Default
	if ImSlice GT n_elements(Slices)-1 then return, Default
	ImOrient = Slices[ImSlice] -> GET('0020'x,'0037'x)
	p0 = Slices[ImSlice] -> GET('0020'x,'0032'x)
	ps0 = Slices[ImSlice] -> GET('0028'x,'0030'x)
	if n_elements(ImOrient) ne 6 then return, Default
	if n_elements(p0) ne 3 then return, Default
	if n_elements(ps0) ne 2 then return, Default
	r0 = ImOrient[0:2]
	c0 = ImOrient[3:5]

	Slices = Localizer -> GET('5200'x,'9230'x, error=error)
	if error EQ 1B then return, Default
	if LocSlice GT n_elements(Slices)-1 then return, Default
	ImOrient = Slices[LocSlice] -> GET('0020'x,'0037'x)
	p1 = Slices[LocSlice] -> GET('0020'x,'0032'x)
	if n_elements(ImOrient) ne 6 then return, Default
	if n_elements(p1) ne 3 then return, Default
	r1 = ImOrient[0:2]
	c1 = ImOrient[3:5]
	n1 = CrossP(c1,r1)

 	return, [total(r0*n1)*ps0[0],total(c0*n1)*ps0[1],total((p0-p1)*n1)]
END

FUNCTION HEADER::GETIMAGE

	im 	= self -> get('7FE0'x,'0010'x)
	if size(im,/n_dimensions) eq 0 then return, im
	ny 	= self -> get('0028'x,'0010'x)
	nx 	= self -> get('0028'x,'0011'x)
	b 	= self -> get('0028'x,'1052'x)
	a 	= self -> get('0028'x,'1053'x)

	im = reform(im,nx,ny,/overwrite)
	im = reverse(im,2)

	if a ne 0 then im = float(a)*float(im) + float(b)
	return, im
END
FUNCTION HEADER::GETVALUE, gr, el, error=error

	error=1B
	i = self->DEindex(gr,el)
	if i eq -1 then return, 0B
	error=0B
	return, (*self.de)[i] -> value()
END
FUNCTION HEADER::GET, gr, el, error=error	;OBSOLETE

	return, self->GetValue(gr,el, error=error)
END
FUNCTION HEADER::TAG, gr=gr, el=el

	n = n_elements(*self.de)
	tag = intarr(n)

	if keyword_set(gr) then for i=0L,n-1 do tag[i]=(*self.de)[i]->gr()
	if keyword_set(el) then for i=0L,n-1 do tag[i]=(*self.de)[i]->el()

	return, tag
END
FUNCTION HEADER::GETVR, gr, el

	i = self->DEindex(gr,el)
	if i eq -1 then return, 'UN'
	return, (*self.de)[i] -> VR()
END








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;                                           ;;;;;;;
;;;;;;             			SET               	 ;;;;;;;
;;;;;;                                           ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;








PRO HEADER::SETIMAGE, im

	d = size(im,/dimensions)
	max = max(im,min=min)
	if max gt min then begin
		a = (2D^(16)-1D)/(max-min)
		b = -min*a
		im = im*a+b
		ra = 1D/a
		rb = -b/a
	endif else begin
		im = im - min
		ra = 1D
		rb = min
	endelse

	im = reverse(fix(im),2)

	self -> set, obj_new('DATA_ELEMENT','0028'x,'0010'x,vr='US',value=uint(d[1]))
	self -> set, obj_new('DATA_ELEMENT','0028'x,'0011'x,vr='US',value=uint(d[0]))
	self -> set, obj_new('DATA_ELEMENT','0028'x,'1052'x,vr='DS',value=ra)
	self -> set, obj_new('DATA_ELEMENT','0028'x,'1053'x,vr='DS',value=rb)
	self -> set, obj_new('DATA_ELEMENT','7FE0'x,'0010'x,vr='OW',value=im)
END

PRO HEADER::SETVALUE, gr, el, value

	v = value
	de = self->de(gr,el)
	if size(de,/type) eq 1 then begin
		self -> set, obj_new('DATA_ELEMENT',gr,el,value=v)
	endif else begin
		de->value, v
	endelse
END
PRO HEADER::SET, de, append=append

	if ptr_valid(self.de) then begin
		i = -1
		if not keyword_set(append) then i=self->DEindex(de->gr(),de->el())
		if i ne -1 then begin
			obj_destroy, (*self.de)[i]
			(*self.de)[i] = de
		endif else *self.de = [*self.de,de]
	endif else $
		if n_elements(de) eq 1 $
		then self.de = ptr_new([de],/no_copy) $
		else self.de = ptr_new(de,/no_copy)

END







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;                                           ;;;;;;;
;;;;;;             		WRITE                    ;;;;;;;
;;;;;;                                           ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;








;Calling sequence "HEADER->WRITETEXT, file" writes the contents to a new file
;Calling sequence "HEADER->WRITETEXT, unit=unit" inserts the contents in an open file
PRO HEADER::WRITETEXT, file, unit=unit, depth=depth

	self -> sort
	if n_elements(file) ne 0 then begin
		get_lun, unit
		openw, unit, file
	endif
	hdr = LMU__DicomTemplate()
	for i=0L,self->n_de()-1 do begin
		de = self->de(i)
		template = Hdr->de(de->gr(),de->el())
		if obj_valid(template) then name=template->name() else name='Unknown data element'
		de->WriteText, unit, name=name, depth=depth
	endfor
	obj_destroy, hdr
	if n_elements(file) ne 0 then begin
		close, unit
		free_lun, unit
	endif
END
PRO HEADER::WRITE, file

	self -> sort
	openw,1,file
	point_lun, 1, 128
	writeu, 1, 'DICM'
	for i=0L,self->n_de()-1 do (self->de(i))->Write, 1
	close, 1
END
PRO HEADER::WRITEDICOM, file  ;Obsolete
	self -> write, file
END









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;                                           ;;;;;;;
;;;;;;             		READ                     ;;;;;;;
;;;;;;                                           ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;









PRO HEADER::READ, file

	self -> Cleanup

	if not PMI__Dicom__Openr(file, unit, TransferSyntaxUID=TS) then return
	ImplicitVR=TS eq '1.2.840.10008.1.2'
	If ImplicitVR then Hdr=LMU__DicomTemplate()
	while not eof(unit) do begin
		de = obj_new('DATA_ELEMENT',unit=unit, TransferSyntaxUID=TS, Template=Hdr)
		self->set, de
	endwhile
	If ImplicitVR then obj_destroy, Hdr
	free_lun, unit
END

PRO HEADER::READDICOM, file

;Reads everything apart from the pixel data

	if not PMI__Dicom__Openr(file,unit, TransferSyntaxUID=TS) then return
	If TS eq '1.2.840.10008.1.2' then Hdr=LMU__DicomTemplate()
	gr=0US & el=0US
	while 1 do begin
		self->set, obj_new('DATA_ELEMENT', unit=unit, TransferSyntaxUID=TS, Template=Hdr), /append
		if eof(unit) then begin
			free_lun, unit
			return
		endif
		readu, unit, gr & readu, unit, el
		point_lun, -unit, p
		point_lun, unit, p-4
		if ((gr eq '7FE0'x) and (el eq '0010'x)) then begin
			free_lun, unit
			return
		endif
	endwhile
END

















;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;                                           ;;;;;;;
;;;;;;             		PRIVATE               	 ;;;;;;;
;;;;;;                                           ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;












PRO HEADER::COPYHEADER, Hdr

	n = Hdr->n_de()
	for i=0L,n-1 do begin
		de = Hdr->de(i)
		self->SetValue, de->gr(), de->el(), de->value()
	endfor
END

PRO HEADER::SORT

	if not ptr_valid(self.de) then return
	*self.de = (*self.de)[sort(self->tag(/gr))]
	groups = reduce(self->tag(/gr),i0,n=ngroups)
	el = self->tag(/el)
	i0 = [i0,n_elements(*self.de)]
	for i=0L,ngroups-1 do begin
		de = (*self.de)[i0[i]:i0[i+1]-1]
		de = de[sort(el[i0[i]:i0[i+1]-1])]
		(*self.de)[i0[i]:i0[i+1]-1] = de
	endfor
END
FUNCTION HEADER::DEindex, gr, el

	for i=0L,self->n_de()-1 do begin
		de = (*self.de)[i]
		if (de->gr() eq gr) and (de->el() eq el) then return, i
	endfor
	return, -1
END
PRO HEADER::SETGROUPLENGTH, gr

	group = self->tag(/gr)
	i = where(group eq gr, cnt)
	n = 0L
	for j=0L,cnt-1 do begin
		n = n + (*self.de)[i[j]]->length()
		vr = (*self.de)[i[j]]->vr()
		if (vr eq 'OB') $
		or (vr eq 'OW') $
		or (vr eq 'OF') $
		or (vr eq 'SQ') $
		or (vr eq 'UT') $
		or (vr eq 'UN') $
		then n=n+12L else n=n+8L
	endfor

	self->set, obj_new('DATA_ELEMENT',gr,'0000'x,vr='UL',value=ulong(n))
END
FUNCTION HEADER::N_DE

	if not ptr_valid(self.de) then return, 0B
	return, n_elements(*self.de)
END
FUNCTION HEADER::DE, i, j

	if n_params() eq 0 then return, *self.de
	if n_params() eq 1 then return, (*self.de)[i]
	k = self->DEindex(i,j)
	if k ne -1 then return, (*self.de)[k]
	return, 0B
END
PRO HEADER::CLEANUP

	if ptr_valid(self.de) then begin
		obj_destroy, *self.de
		ptr_free, self.de
	endif
END
FUNCTION HEADER::INIT, file=file

	if n_elements(file) ne 0 then begin

		self->Read, file
		return, 1B
	endif

	return, 1B
END
PRO HEADER__DEFINE

	struct = {HEADER					$
	,	De	:	ptr_new()		}	;pointer to an array of objects of type DATA_ELEMENT
END