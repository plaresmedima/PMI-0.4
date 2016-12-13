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


PRO PMI__Display__Image::Set, $
 	PMI__REFRESH=pmi__refresh, PMI__RESIZE=pmi_resize, $
 	Refresh=Refresh, Erase=Erase, $
 	xsize=xsize,ysize=ysize, $
 	Image=Image

	if keyword_set(pmi_resize) then begin
		gpmi = widget_info(/geometry, tlb(self.id))
		self -> Set, xsize=gpmi.xsize, ysize=gpmi.ysize, /Refresh, /Erase
	endif

	if n_elements(xsize) ne 0 $
	or n_elements(ysize) ne 0 $
	then Self.Image->SET,xsize=xsize, ysize=ysize-10

	if n_elements(Image) ne 0 then Self.Image -> SET, Image=Image

	if keyword_set(Refresh) then self.Image -> Set, /Refresh, Erase=Erase
END
PRO PMI__Display__Image::Get, $
 	CursorPos=CursorPos

	if arg_present(CursorPos) then CursorPos=self.CursorPos
END

PRO PMI__Display__Image::Cleanup
	obj_destroy, self.Image
	widget_control, self.id, /destroy
END

FUNCTION PMI__Display__Image::Init, parent, CursorPos, xsize=xsize, ysize=ysize

	IF n_elements(CursorPos) NE 0 THEN self.CursorPos = CursorPos

	self.id = widget_base(parent,/column,map=0)
	self.Image = Obj_new('PMI__Module__Image',self.id)
	Self.Image -> SET, Image=bytarr(3,256,256)
	Self -> Set, xsize=xsize, ysize=ysize, /refresh
	widget_control, self.id, set_uvalue = self, /map
	return, 1
END

PRO PMI__Display__Image__Define

	PMI__Module__Image__Define

	struct = {PMI__Display__Image, $
		id:0L,$
		CursorPos:lonarr(4),$
		Image:obj_new()}
END

