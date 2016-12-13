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


PRO PMI__Module__MessageBar::Set, Msg, f, PixelValue=pv

	if n_elements(pv) ne 0 then begin
		x=strcompress(floor(pv[0]),/remove_all)
		y=strcompress(floor(pv[1]),/remove_all)
		z=strcompress(pv[2],/remove_all)
		t=strcompress(pv[4],/remove_all) + ' + ' + strcompress(pv[3]-pv[4],/remove_all)
		v=strcompress(pv[5],/remove_all)
		Msg = 'x: ' + x + '    y: ' + y + '    z: ' + z + '    t: ' + t + '     Pixel Value  ' + v
	endif

	if n_elements(Msg) eq 0 then begin
		self.counter = 0E
		msg = 'Bored..'
	endif else begin
		if n_elements(f) ne 0 then begin
			if f eq 0 then begin
				self.counter=0E
				msg = msg + ' (0 %)'
			endif
			if not finite(f) then f=0E
			perc = floor(f*100E)
			if perc - self.counter ge 1 then begin
				self.counter = perc
				perc = strcompress(perc,/remove_all)
				msg = msg + ' (' + perc + ' %)'
			endif else return
		endif else self.counter = 0E
	endelse

	widget_control, self.id, set_value = Msg
END



PRO PMI__Module__MessageBar::Cleanup
END
FUNCTION PMI__Module__MessageBar::Init, parent

	self.id = widget_label(parent,/dynamic_resize,uname='PMI__Module__MessageBoard',/align_center)
	widget_control, self.id, set_uvalue=self

	return, 1B
END
PRO PMI__Module__MessageBar__Define

	struct = {PMI__Module__MessageBar 	$
	,	id	  	: 0L $
	,	counter : 0E $
	}
END