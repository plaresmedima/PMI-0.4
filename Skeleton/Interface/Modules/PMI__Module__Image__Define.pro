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


PRO PMI__Module__Image::GET, $
	ScrSize=ScrSize,$
	Dimensions=Dimensions,$
	Scale=Scale, $
	ImagePos=ImagePos

	if arg_present(ScrSize) then begin
		g = widget_info(self.id,/geometry)
		ScrSize = [g.scr_xsize,g.scr_ysize]
	endif
	if arg_present(Dimensions) then begin
		if ptr_valid(self.img) then begin
			Dimensions = size(*self.img,/dimensions)
			Dimensions = Dimensions[1:2]
		endif else self -> Get, ScrSize=Dimensions
	endif
	if arg_present(Scale) then begin
		self -> Get, ScrSize=w, Dimensions=d
		Scale = min(w/d,i)
	endif
	if arg_present(ImagePos) then begin
		self -> Get, ScrSize=w, Dimensions=d
		s = min(w/d,i)
		ImagePos = [0L,0L]
		ImagePos[1-i] = (w[1-i]-s*d[1-i])/2
	endif

END

PRO PMI__Module__Image::SET $
, 	xsize=xsize, ysize=ysize $
, 	Refresh=Refresh, Erase=Erase $
,	Image = Image, ScaleImage=ScaleImage

	if n_elements(xsize) ne 0 $
	or n_elements(ysize) ne 0 $
	then begin
		widget_control, self.id, scr_xsize=xsize, scr_ysize=ysize
		self->SET, /ScaleImage
	endif

	if n_elements(image) ne 0 then begin
		ptr_free, self.img
		self.img = ptr_new(image)
		Self -> Set, /ScaleImage
	endif
	if keyword_set(ScaleImage) and ptr_valid(self.img) then begin
		Self->Get, Scale=s
		d = s*size(*self.img,/dimensions)
		d = d[1:2]
		scl =  bytarr(3,d[0],d[1])
		FOR i=0L,2 DO $
			 scl[i,*,*] = congrid(reform((*self.img)[i,*,*]),d[0],d[1])
		ptr_free, self.scl
		self.scl=ptr_new(scl,/no_copy)
	endif
	if keyword_set(Refresh) and ptr_valid(self.scl) then begin
		widget_control, self.id, get_value = win
		wset, win
		if keyword_set(erase) then erase
		self -> Get, ImagePos=r
		tv, *self.scl, /true, r[0], r[1]
	endif
END

PRO PMI__Module__Image::Cleanup
	ptr_free, self.img, self.scl
END
FUNCTION PMI__Module__Image::Init, parent, uname=uname

	self.id	= widget_draw(parent,/retain, uname=uname)
	widget_control, self.id, set_uvalue = self
	return, 1B
END
PRO PMI__Module__Image__Define

	struct = {PMI__Module__Image, $
		id:0L, $
		img:ptr_new(),$	;	24bit image
		scl:ptr_new()}	;	24bit Scaled Displayed image

END