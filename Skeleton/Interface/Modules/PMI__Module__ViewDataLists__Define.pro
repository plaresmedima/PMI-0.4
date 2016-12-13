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


FUNCTION PMI__Module__ViewDataLists::Event, ev

	i = where(ev.id EQ self.List) & i=i[0]

	IF ev.index EQ -1 THEN BEGIN
		IF ev.str NE widget_info(ev.id,/combobox_gettext) THEN return, 0
		IF ev.str EQ '' THEN BEGIN
			self->SET, /REFRESH
			return,0
		ENDIF
		IF self.sel[i] EQ 0 THEN BEGIN
			self->SET, /REFRESH
			return,0
		ENDIF
		self->GET, o, OBJECT=i, STDY=s
		o->name, ev.str
		s->saved, 0
		self->SET, LIST=i
		return, {id:self.id, top:ev.top, handler:0L, name:'PMI__Module__ViewDataLists', list:i, index:self.sel[i]}
	ENDIF

	IF i EQ 0 THEN BEGIN	;User selected a different study
		self.sel[0] = ev.index
		self->SET, /SER_REG_INIT
		for k=1,2 DO self->SET, LIST=k
	ENDIF ELSE self.sel[i] = ev.index	;User selected a different series (i=1) or region (i=2)

	return, {id:self.id, top:ev.top, handler:0L, name:'PMI__Module__ViewDataLists', list:i, index:ev.index}
END
FUNCTION PMI__Module__ViewDataLists__Event, ev
	widget_control, ev.handler, get_uvalue=self
	return, Self -> Event(ev)
END





PRO PMI__Module__ViewDataLists::Set, $
	XSIZE = xsize, $
	STDY = stdy, $
	SERIES = series, $
	REGION = region, $
	SER_REG_INIT = ser_reg_init, $
	D3_SER_REG_INIT = d3_ser_reg_init, $
	INIT = init, $
	D3_INIT = D3_INIT, $
	REFRESH = refresh, $
	LIST = list

	if n_elements(xsize) ne 0 then begin
		if xsize lt 100 then xsize=100
		for i=0L,2 do widget_control, Self.List[i], xsize=xsize-80
	endif

	IF n_elements(stdy) NE 0 THEN BEGIN
		i = WHERE(*(self.ptr[0]) EQ stdy, cnt)
		IF cnt GT 0 THEN self.sel[0]=i[0] ELSE self.sel[0]=0
		self->SET, /SER_REG_INIT
	ENDIF

	IF n_elements(series) NE 0 THEN BEGIN
		i = WHERE(*(self.ptr[1]) EQ series, cnt)
		IF cnt GT 0 THEN self.sel[1] = i[0] ELSE self.sel[1]=0
	ENDIF

	IF n_elements(region) NE 0 THEN BEGIN
		i = WHERE(*(self.ptr[2]) EQ region, cnt)
		IF cnt GT 0 THEN self.sel[2] = i[0] ELSE self.sel[2]=0
	ENDIF

	IF keyword_set(d3_ser_reg_init) THEN BEGIN;only show series with s->d(3) > 1
		PMI__INFO, tlb(self.id), Series=S_Sel ; get selected series
		self->GET, STDY=s
		IF obj_valid(s) THEN BEGIN
			; set series
			self.sel[1] = 0
			Series = s->getvalue(0)
			sizes = intarr(n_elements(Series))
			for i = 0,n_elements(Series) -1 do begin
				sizes(i) = Series(i)->d(3)
			endfor
			indices = where(sizes gt 1, count)
			if count gt 0 then begin
				*(self.ptr[1]) = [obj_new(),Series(indices)]
				;check if currently selected series is dynamic
				for i =0,n_elements(indices)-1 do begin
					if strcmp(Series(indices(i))->name(),S_sel->name()) then count = i+1
				endfor
				self.sel[1] = count
			endif else *(self.ptr[1]) = [obj_new()]
			; set regions
			self.sel[2] = 1 + s->sel(1)
			*(self.ptr[2]) = s->getvalue(1)
			IF s->n(1) GT 0 THEN *(self.ptr[2]) = [obj_new(),*(self.ptr[2])]
		ENDIF ELSE BEGIN
			self.sel[1:2] = 0
			*(self.ptr[1:2]) = [obj_new()]
		ENDELSE
	ENDIF

	IF keyword_set(ser_reg_init) THEN BEGIN
		self->GET, STDY=s
		FOR j=0,1 DO BEGIN
			IF obj_valid(s) THEN BEGIN
				self.sel[j+1] = 1 + s->sel(j)
				*(self.ptr[j+1]) = s->getvalue(j)
				IF s->n(j) GT 0 THEN *(self.ptr[j+1]) = [obj_new(),*(self.ptr[j+1])]
			ENDIF ELSE BEGIN
				self.sel[j+1] = 0
				*(self.ptr[j+1]) = [obj_new()]
			ENDELSE
		ENDFOR
	ENDIF

	IF keyword_set(init) THEN BEGIN ;set the values of lists and selection to those in the State Objects
		PMI__INFO, tlb(self.id), STATE=s
		self.sel[0] = 1 + s->sel()
		*(self.ptr[0]) = s->getvalue()
		IF s->n() GT 0 THEN *(self.ptr[0]) = [obj_new(),*(self.ptr[0])]
		self -> SET, /SER_REG_INIT
	ENDIF

	IF keyword_set(D3_init) THEN BEGIN ;set the values of lists and selection to those in the State Objects
		PMI__INFO, tlb(self.id), STATE=s
		self.sel[0] = 1 + s->sel()
		*(self.ptr[0]) = s->getvalue()
		IF s->n() GT 0 THEN *(self.ptr[0]) = [obj_new(),*(self.ptr[0])]
		self -> SET, /D3_SER_REG_INIT
	ENDIF

	IF n_elements(list) ne 0 THEN BEGIN
		n = n_elements(*(self.ptr[list]))
		names = strarr(n)
		names[0] = '<empty>'
		FOR i=1L,n-1 DO names[i] = (*(self.ptr[list]))[i]->name()
		widget_control, self.List[list], SET_VALUE=Names, SET_COMBOBOX_SELECT=self.sel[list]
		IF list EQ 0 $
			THEN widget_control, self.List[list], SENSITIVE = n GT 1 $
			ELSE widget_control, self.List[list], SENSITIVE = (n GT 1) AND (self.sel[0] GT 0)
	ENDIF

	IF keyword_set(refresh) THEN FOR i=0,2 DO self->SET, LIST=i
END





PRO PMI__Module__ViewDataLists::GET, return_value, $
	STDY = stdy, $
	SERIES = series, $
	REGION = region, $
	OBJECT = object

	IF n_elements(object) NE 0 THEN $
		return_value = (*(self.ptr[object]))[self.sel[object]]

	IF arg_present(stdy) THEN self->GET, stdy, OBJECT=0
	IF arg_present(series) THEN self->GET, series, OBJECT=1
	IF arg_present(region) THEN self->GET, region, OBJECT=2
END


PRO PMI__Module__ViewDataLists::Cleanup
	ptr_free, self.ptr
END
FUNCTION PMI__Module__ViewDataLists::Init, parent, ysize=ysize

	self.ptr = ptrarr(3,/allocate_heap)
	self.id = widget_base(parent,/column,event_func='PMI__Module__ViewDataLists__Event',map=0,/frame, ysize=ysize)

	Labels = ['Study > ','Series > ','Region > ']
	editable = [0,1,1]
	FOR i=0,2 DO BEGIN
		id = widget_base(self.id,/ROW)
		Sid = widget_label(id, VALUE=Labels[i], XSIZE=50)
		self.List[i] = widget_combobox(id, /DYNAMIC_RESIZE, EDITABLE=editable[i])
	ENDFOR
	widget_control, self.id, set_uvalue = self, /map
	return, 1B
END



PRO PMI__Module__ViewDataLists__Define

	struct = {PMI__Module__ViewDataLists, 	$
		id		:0L,$
		List	:lonarr(3),$
		Sel:lonarr(3),$	;indices of selected Study, Series, Region
		ptr:ptrarr(3)}	;pointers to arrays of all Studies, Series, Regions in the lists
END