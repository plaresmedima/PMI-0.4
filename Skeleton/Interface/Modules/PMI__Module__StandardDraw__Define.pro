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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;                                           ;;;;;;;
;;;;;;             EVENT FUNCTIONS               ;;;;;;;
;;;;;;                                           ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






FUNCTION PMI__Module__StandardDraw::KeyBoardNavigator, ev

	if ev.press eq 0 then return, 0B
	if (ev.key lt 5) or (ev.key gt 8) then return, 0B
	case ev.key of
		5: begin & dim=3 & incr=-1 & end
		6: begin & dim=3 & incr=+1 & end
		7: begin & dim=2 & incr=+1 & end
		8: begin & dim=2 & incr=-1 & end
	endcase
	self.CursorPos[dim] = self.CursorPos[dim] + incr
	self -> Set, /UpdateCursorPos, /LoadData, /Refresh
	ev.id=self.id & return, ev
END

FUNCTION PMI__Module__StandardDraw::MeasureAngle, ev
	return, 0B
END
FUNCTION PMI__Module__StandardDraw::MeasureProfile, ev
	return, 0B
END
FUNCTION PMI__Module__StandardDraw::RotateRegion, ev

	if ev.type eq 6 then return, self->KeyBoardNavigator(ev)
	if ev.type eq 5 then return, 0B

	case ev.type of
	0: self.value = ptr_new([ev.x,ev.y]) ;Press
	1: ptr_free, self.value ;Release
	2: 	begin ;Motion
		self.CursorPos[0:1] = self->GetPosition(ev.x,ev.y)
		if ptr_valid(self.value) then begin
			r0 = *self.value
			Bin = RotateBinary(*self.bin,r0,[ev.x,ev.y])
			self -> SetRegion, Bin, /new
		endif
		end
	endcase
	ev.id=self.id & RETURN, ev
END
FUNCTION PMI__Module__StandardDraw::ShiftRegion, ev

	if ev.type eq 6 then return, self->KeyBoardNavigator(ev)
	if ev.type eq 5 then return, 0B

	if ev.type eq 0 then self.value = ptr_new(self->GetPosition(ev.x,ev.y)) ;Press
	if ev.type eq 1 then ptr_free, self.value ;Release
	if ev.type eq 2 then begin ;Motion
		self.CursorPos[0:1] = self->GetPosition(ev.x,ev.y)
		if ptr_valid(self.value) then begin
			r0 = *self.value
			*self.value = self.CursorPos[0:1]
			p = *self.value - r0
			if (p[0] ne 0) or (p[1] ne 0) then begin
				Bin = shift(*self.bin,p[0],p[1])
				self -> SetRegion, Bin, /new
			endif
		endif
	endif
	ev.id=self.id & RETURN, ev
END



FUNCTION PMI__Module__StandardDraw::AHASegment, ev

	if ev.type eq 6 then return, self->KeyBoardNavigator(ev)
	if ev.type eq 5 then return, 0B

	p1 = self -> GetPosition(ev.x,ev.y,scr_r=r1)

	case ev.type of
	0: if ev.clicks eq 1 then begin ;Press

		;CREATE SEGMENTS

		If not obj_valid(self.region) then begin
		    msg = ['No region selected', 'Please select an ROI and try again']
			OK = dialog_message(msg,/information)
			ev.id=self.id & RETURN, ev
		endif

		bin = Self.Region -> Read(Self.Stdy->DataPath(),Self.CursorPos[2],Self.CursorPos[3])
		case ev.press of
			4: begin
			   split_into_four_segments, bin, p1, seg1=bin1, seg2=bin2, seg3=bin3, seg4=bin4
			   bin5 = bin*0
			   bin6 = bin*0
			end
			else: split_into_six_segments, bin, p1, seg1=bin1, seg2=bin2, seg3=bin3, seg4=bin4, seg5=bin5, seg6=bin6
		endcase

		;EXPORT STATISTICS

		ind1 = where(bin1 EQ 1, cnt1)
		ind2 = where(bin2 EQ 1, cnt2)
		ind3 = where(bin3 EQ 1, cnt3)
		ind4 = where(bin4 EQ 1, cnt4)
		ind5 = where(bin5 EQ 1, cnt5)
		ind6 = where(bin6 EQ 1, cnt6)

        d = Self.Series->d()
        v = strarr(1+d[2],1+6)
        for i=0L, n_elements(v)-1 do v[i] = 'TBD'
        for i=1L, 6 do v[0,i] = 'Segment ' + strcompress(i,/remove_all)
        for i=1L, d[2] do v[i,0] = 'Slice ' + strcompress(i-1,/remove_all)

        Path = Self.Stdy->DataPath() + 'AHA segments'
        file_mkdir, Path
        FilenameMEAN = Path + '\' + cleanstr(Self.Series->name() + '_' + Self.Region->name()) + '_AHAsegments_MEAN.csv'
        FilenameMEDIAN = Path + '\' + cleanstr(Self.Series->name() + '_' + Self.Region->name()) + '_AHAsegments_MEDIAN.csv'

		if PMI__read_csv(FilenameMEAN, v_avr) eq 0 then begin
			v_avr = v
			v_avr[0,0] = 'MEAN'
		endif
		if PMI__read_csv(FilenameMEDIAN, v_med) eq 0 then begin
			v_med = v
			v_med[0,0] = 'MEDIAN'
		endif

        image = Self.Series->Read(Self.Stdy->DataPath(),Self.CursorPos[2],Self.CursorPos[3])

		if cnt1 GT 0 then begin
			v_avr[1+Self.CursorPos[2],1] = strcompress(total(image[ind1])/cnt1,/remove_all)
			v_med[1+Self.CursorPos[2],1] = strcompress(median(image[ind1]),/remove_all)
		endif else begin
			v_avr[1+Self.CursorPos[2],1] = 'N/A'
			v_med[1+Self.CursorPos[2],1] = 'N/A'
		endelse
		if cnt2 GT 0 then begin
			v_avr[1+Self.CursorPos[2],2] = strcompress(total(image[ind2])/cnt2,/remove_all)
			v_med[1+Self.CursorPos[2],2] = strcompress(median(image[ind2]),/remove_all)
		endif else begin
			v_avr[1+Self.CursorPos[2],2] = 'N/A'
			v_med[1+Self.CursorPos[2],2] = 'N/A'
		endelse
		if cnt3 GT 0 then begin
			v_avr[1+Self.CursorPos[2],3] = strcompress(total(image[ind3])/cnt3,/remove_all)
			v_med[1+Self.CursorPos[2],3] = strcompress(median(image[ind3]),/remove_all)
		endif else begin
			v_avr[1+Self.CursorPos[2],3] = 'N/A'
			v_med[1+Self.CursorPos[2],3] = 'N/A'
		endelse
		if cnt4 GT 0 then begin
			v_avr[1+Self.CursorPos[2],4] = strcompress(total(image[ind4])/cnt4,/remove_all)
			v_med[1+Self.CursorPos[2],4] = strcompress(median(image[ind4]),/remove_all)
		endif else begin
			v_avr[1+Self.CursorPos[2],4] = 'N/A'
			v_med[1+Self.CursorPos[2],4] = 'N/A'
		endelse
		if cnt5 GT 0 then begin
			v_avr[1+Self.CursorPos[2],5] = strcompress(total(image[ind5])/cnt5,/remove_all)
			v_med[1+Self.CursorPos[2],5] = strcompress(median(image[ind5]),/remove_all)
		endif else begin
			v_avr[1+Self.CursorPos[2],5] = 'N/A'
			v_med[1+Self.CursorPos[2],5] = 'N/A'
		endelse
		if cnt6 GT 0 then begin
			v_avr[1+Self.CursorPos[2],6] = strcompress(total(image[ind6])/cnt6,/remove_all)
			v_med[1+Self.CursorPos[2],6] = strcompress(median(image[ind6]),/remove_all)
		endif else begin
			v_avr[1+Self.CursorPos[2],6] = 'N/A'
			v_med[1+Self.CursorPos[2],6] = 'N/A'
		endelse

		pmi__write_csv, FilenameMEAN, v_avr, ERROR=Err
		If Err ne 0 then begin
		    msg = ['Could not write MEAN values to file', $
		      'If the file is open, close it and try again']
			OK = dialog_message(msg,/information)
			ev.id=self.id & RETURN, ev
		endif
		pmi__write_csv, FilenameMEDIAN, v_med, ERROR=Err
		If Err ne 0 then begin
		    msg = ['Could not write MEDIAN values to file', $
		      'If the file is open, close it and try again']
			OK = dialog_message(msg,/information)
			ev.id=self.id & RETURN, ev
		endif

		;SAVE SEGMENTS AS ROIs

        roi_sel = Self.Stdy->sel(1)

		Roi1 = Self.Stdy -> New('REGION', $
		  Name = self.Region->Name() + '_[slice_'+strcompress(Self.CursorPos[2], /remove_all) + '_seg_'+strcompress(1, /remove_all) + ']', $
		  Domain = self.Region->dom(), Color = self.Region->Clr())
		Roi2 = Self.Stdy -> New('REGION', $
		  Name = self.Region->Name() + '_[slice_'+strcompress(Self.CursorPos[2], /remove_all) + '_seg_'+strcompress(2, /remove_all) + ']', $
		  Domain = self.Region->dom(), Color = self.Region->Clr())
		Roi3 = Self.Stdy -> New('REGION', $
		  Name = self.Region->Name() + '_[slice_'+strcompress(Self.CursorPos[2], /remove_all) + '_seg_'+strcompress(3, /remove_all) + ']', $
		  Domain = self.Region->dom(), Color = self.Region->Clr())
		Roi4 = Self.Stdy -> New('REGION', $
		  Name = self.Region->Name() + '_[slice_'+strcompress(Self.CursorPos[2], /remove_all) + '_seg_'+strcompress(4, /remove_all) + ']', $
		  Domain = self.Region->dom(), Color = self.Region->Clr())
		if ev.press ne 4 then Roi5 = Self.Stdy -> New('REGION', $
		  Name = self.Region->Name() + '_[slice_'+strcompress(Self.CursorPos[2], /remove_all) + '_seg_'+strcompress(5, /remove_all) + ']', $
		  Domain = self.Region->dom(), Color = self.Region->Clr())
		if ev.press ne 4 then Roi6 = Self.Stdy -> New('REGION', $
		  Name = self.Region->Name() + '_[slice_'+strcompress(Self.CursorPos[2], /remove_all) + '_seg_'+strcompress(6, /remove_all) + ']', $
		  Domain = self.Region->dom(), Color = self.Region->Clr())

		Roi1 -> Write, Self.Stdy->DataPath(), Bin1, self.CursorPos[2], self.CursorPos[3]
		Roi2 -> Write, Self.Stdy->DataPath(), Bin2, self.CursorPos[2], self.CursorPos[3]
		Roi3 -> Write, Self.Stdy->DataPath(), Bin3, self.CursorPos[2], self.CursorPos[3]
		Roi4 -> Write, Self.Stdy->DataPath(), Bin4, self.CursorPos[2], self.CursorPos[3]
		if ev.press ne 4 then Roi5 -> Write, Self.Stdy->DataPath(), Bin5, self.CursorPos[2], self.CursorPos[3]
		if ev.press ne 4 then Roi6 -> Write, Self.Stdy->DataPath(), Bin6, self.CursorPos[2], self.CursorPos[3]

        self.Stdy -> Saved, 0
        Self.Stdy->sel, 1, roi_sel
		PMI__control, ev.top, /refresh

		;VISUAL FEEDBACK - PLOT STAR ON IMAGE

        if ev.press eq 4 then angle=90. else angle=60.

  		c = cos(angle*!PI/180)
  		s = sin(angle*!PI/180)

		p0 = centroid(bin)

		v1 = p1-p0
  		v2 = [c*v1[0]-s*v1[1], s*v1[0]+c*v1[1]]
  		v3 = [c*v2[0]-s*v2[1], s*v2[0]+c*v2[1]]
  		v4 = [c*v3[0]-s*v3[1], s*v3[0]+c*v3[1]]
  		if ev.press ne 4 then v5 = [c*v4[0]-s*v4[1], s*v4[0]+c*v4[1]]
  		if ev.press ne 4 then v6 = [c*v5[0]-s*v5[1], s*v5[0]+c*v5[1]]

		r0 = self -> ScreenPosition(p0)
;		r1 = self -> ScreenPosition(p0+v1)
		r2 = self -> ScreenPosition(p0+v2)
		r3 = self -> ScreenPosition(p0+v3)
		r4 = self -> ScreenPosition(p0+v4)
		if ev.press ne 4 then r5 = self -> ScreenPosition(p0+v5)
		if ev.press ne 4 then r6 = self -> ScreenPosition(p0+v6)

		plots, [r0[0],r1[0]], [r0[1],r1[1]], /device
		plots, [r0[0],r2[0]], [r0[1],r2[1]], /device
		plots, [r0[0],r3[0]], [r0[1],r3[1]], /device
		plots, [r0[0],r4[0]], [r0[1],r4[1]], /device
		if ev.press ne 4 then plots, [r0[0],r5[0]], [r0[1],r5[1]], /device
		if ev.press ne 4 then plots, [r0[0],r6[0]], [r0[1],r6[1]], /device

	endif
	1: ;Release
	2: self.CursorPos[0:1] = p1 ;Motion
	endcase
	ev.id=self.id & RETURN, ev
END

FUNCTION PMI__Module__StandardDraw::RegionPixel, ev

	if ev.type eq 6 then return, self->KeyBoardNavigator(ev)
	if ev.type eq 5 then return, 0B

	p1 = self -> GetPosition(ev.x,ev.y,scr_r=r1)

	case ev.type of
	0: if ev.clicks eq 2 then begin ;Press
		d = Self.Series->d()
		bin = bytarr(d[0],d[1])
		bin[p1[0],p1[1]] = 1B
		self -> SetRegion, Bin
		endif
	1: ;Release
	2: self.CursorPos[0:1] = p1 ;Motion
	endcase
	ev.id=self.id & RETURN, ev
END

FUNCTION PMI__Module__StandardDraw::RegionRectangle, ev

	if ev.type eq 6 then return, self->KeyBoardNavigator(ev)
	if ev.type eq 5 then return, 0B

	p1 = self -> GetPosition(ev.x,ev.y,scr_r=r1)

	case ev.type of
	0: self.value = ptr_new(r1) ;Press
	2: begin ;Motion
		self.CursorPos[0:1] = p1
		if ptr_valid(self.value) then begin
			self -> Set, /Refresh
			r0 = *self.value
			plots, [r0[0],r0[0],r1[0],r1[0],r0[0]], [r0[1],r1[1],r1[1],r0[1],r0[1]], /device
		endif
		end
	1: if ptr_valid(self.value) then begin ;Release
		r0 = *self.value
		p0 = self -> GetPosition(r0[0],r0[1])
		x0 = min([p0[0],p1[0]],max=x1) & x1=x1+1
		y0 = min([p0[1],p1[1]],max=y1) & y1=y1+1
		if (x1-x0 gt 1) and (y1-y0 gt 1) then begin
			d = self.Series->d()
			bin = bytarr(d[0],d[1])
			bin[polyfillv([x0,x1,x1,x0],[y0,y0,y1,y1],d[0],d[1])] = 1B
			self -> SetRegion, Bin
		endif else self -> Set, /Refresh
		ptr_free, self.value
		endif
	endcase
	ev.id=self.id & RETURN, ev
END


;This event procedure is executed when Tool = 'RegionCircle'
;The event is first processed, then passed on to the display

FUNCTION PMI__Module__StandardDraw::RegionCircle, ev

	if ev.type eq 6 then return, self->KeyBoardNavigator(ev) ;Upon key press (arrows up/down/left/right), execute the keyboard event
	if ev.type eq 5 then return, 0B ;Ignore key press of non-asci characters (shift, ctrl, alt, ...)

	p1 = self -> GetPosition(ev.x,ev.y,scr_r=r1)
	; p1 = cursor position in the image
	; r1 = cursor position in the display

	case ev.type of
	0: self.value = ptr_new(r1)	;Upon button press, store the cursor position in self.value
	2: begin ;Motion event
		self.CursorPos[0:1] = p1	;modify the cursorposition
		;if the motion happens when the button is pressed
		;then draw a circle with a center in the position r0 where the button was pressed (stored in self.value)
		;otherwise do nothing
		if ptr_valid(self.value) then begin
			self -> Set, /Refresh
			r0 = *self.value
			r1 = PMI__Circle(norm(r1-r0),r0)
			plots, r1[*,0], r1[*,1], /device
		endif
		end
	1: if ptr_valid(self.value) then begin
		;Upon release: check if the cursor has moved more than one pixel since button press
		;If it has, add the circle to the Region
		;Otherwise, just clear self.value.
		r0 = *self.value
		p0 = self -> GetPosition(r0[0],r0[1])
		rad = norm(p1-p0)
		if rad gt 0 then begin
			d = self.Series->d()
			bin = bytarr(d[0],d[1])
			p1 = PMI__Circle(rad,p0)
			bin[polyfillv(p1[*,0],p1[*,1],d[0],d[1])] = 1B
			self->SetRegion, Bin
		endif else self -> Set, /Refresh
		ptr_free, self.value
		endif
	endcase
	ev.id=self.id & RETURN, ev ;pass on the event to the next level in the hierarchy
END


FUNCTION PMI__Module__StandardDraw::RegionPolygon, ev

	if ev.type eq 6 then return, self->KeyBoardNavigator(ev)
	if ev.type eq 5 then return, 0B

	p1 = self -> GetPosition(ev.x,ev.y,scr_r=r1)
	r1 = transpose(r1)

	case ev.type of
	0: case ev.clicks of ;Press
		1: 	if not ptr_valid(self.value) then begin
				r = make_array(1,2)
				r[0,*] = r1
				self.value = ptr_new(r)
			endif else *self.value = [*self.value,r1]
		2: begin
			r = *self.value
			n = n_elements(r[*,0])
			if n gt 2 then begin
				for i=0L,n-1 do r[i,*] = self->GetPosition(r[i,0],r[i,1])
				d = Self.Series->d()
				bin = bytarr(d[0],d[1])
				v = polyfillv(r[*,0],r[*,1],d[0],d[1])
				if n_elements(v) gt 1 then bin[v] = 1B
				self -> SetRegion, Bin
			endif else self -> Set, /Refresh
			ptr_free, self.value
			end
		endcase
	2: begin ;Motion
		self.CursorPos[0:1] = p1
		if ptr_valid(self.value) then begin
			self -> Set, /Refresh
			r = *self.value
			plots, transpose([r,r1]), /device
		endif
		end
	1: ;Release
	endcase
	ev.id=self.id & RETURN, ev
END

FUNCTION PMI__Module__StandardDraw::MeasureDistance, ev

	if ev.type eq 6 then return, self->KeyBoardNavigator(ev)
	if ev.type eq 5 then return, 0B

	p1 = self -> GetPosition(ev.x,ev.y,scr_r=r1)
	r1 = transpose(r1)

	case ev.type of
	0: case ev.clicks of ;Press
		1: 	if not ptr_valid(self.value) then begin
				r = make_array(1,2)
				r[0,*] = r1
				self.value = ptr_new(r)
			endif else *self.value = [*self.value,r1]
		2: begin
			r = *self.value
			n = n_elements(r[*,0])
			for i=0L,n-1 do r[i,*] = self->GetPosition(r[i,0],r[i,1])
			length = 0E
			for i=1L,n-1 do length = length + sqrt(total((r[i,*]-r[i-1,*])^2))
			ptr_free, self.value
			pixelsize = self.series -> get('0028'x,'0030'x)
			pixelsize = pixelsize[0]
			units = 'mm'
			if pixelsize eq 0B then begin
				pixelsize = 1
				units = 'pixel sizes'
			endif
			msg = 'Line length = ' + strcompress(pixelsize*length,/remove_all) + ' ' + units
			ok = dialog_message(msg,/information)
			self -> Set, /Refresh
			end
		endcase
	2: begin ;Motion
		self.CursorPos[0:1] = p1
		if ptr_valid(self.value) then begin
			self -> Set, /Refresh
			r = *self.value
			plots, transpose([r,r1]), /device
		endif
		end
	1: ;Release
	endcase
	ev.id=self.id & RETURN, ev
END

;OLD VERSION: ONLY MEASURES ONE STRAIGHT LINE
;FUNCTION PMI__Module__StandardDraw::MeasureDistanceOld, ev
;
;	if ev.type eq 6 then return, self->KeyBoardNavigator(ev)
;	if ev.type eq 5 then return, 0B
;
;	p1 = self -> GetPosition(ev.x,ev.y,scr_r=r1)
;
;	case ev.type of
;	0: self.value = ptr_new(r1) ;Press
;	2: begin ;Motion
;		self.CursorPos[0:1] = p1
;		if ptr_valid(self.value) then begin
;			self -> Set, /Refresh
;			r0 = *self.value
;			plots, [r0[0],r1[0]], [r0[1],r1[1]], /device
;		endif
;		end
;	1: begin ;Release
;		r0 = *self.value
;		p0 = self -> GetPosition(r0[0],r0[1])
;		length = sqrt(total((p1-p0)^2))
;		length = strcompress(length,/remove_all)
;		msg = 'Line length = ' + length + ' pixel sizes'
;		ok = dialog_message(msg,/information)
;		self -> Set, /Refresh
;		ptr_free, self.value
;		end
;	endcase
;	ev.id=self.id & RETURN, ev
;END

FUNCTION PMI__Module__StandardDraw::RegionDisplay, ev

	if ev.type eq 6 then return, self->KeyBoardNavigator(ev)
	if ev.type eq 5 then return, 0B

	p1 = self -> GetPosition(ev.x,ev.y,scr_r=r1)

	case ev.type of
	0: 	self.value = ptr_new(transpose(r1)) ;Press
	2:  BEGIN ;Motion
		self.CursorPos[0:1] = p1
		if ptr_valid(self.value) then begin
			self -> Set, /Refresh
			r = [*self.value,transpose(r1)]
			*self.value = r
			plots, transpose(r), /device
		endif
		END
	1:  if ptr_valid(self.value) then begin ;Release
		r = *self.value
		n = n_elements(r[*,0])
		if n gt 2 then begin
			for i=0L,n-1 do r[i,*] = self->GetPosition(r[i,0],r[i,1])
			d = self.Series -> d()
			bin = bytarr(d[0],d[1])
			v = polyfillv(r[*,0],r[*,1],d[0],d[1])
			if n_elements(v) gt 0 then bin[v] = 1B
			self -> SetRegion, Bin
		endif else self -> Set, /Refresh
		ptr_free, self.value
		ENDIF
	endcase
	ev.id=self.id & RETURN, ev
END



FUNCTION PMI__Module__StandardDraw::ZoomIn, ev

	if ev.type eq 6 then return, self->KeyBoardNavigator(ev)
	if ev.type eq 5 then return, 0B

	p1 = self -> GetPosition(ev.x,ev.y,scr_r=r1)

	case ev.type of
	0: 	if ev.clicks eq 1 then self.value = ptr_new(r1) $ Press
		else Self -> Set, ZoomWindow=lonarr(4), /Refresh, /erase
	2:	begin ;Motion
		if ptr_valid(self.value) then begin
			Self -> Set, /Refresh
			r0 = *self.value
			plots, [r0[0],r0[0],r1[0],r1[0],r0[0]], [r0[1],r1[1],r1[1],r0[1],r0[1]], /device
		endif
		self.CursorPos[0:1] = p1
		end
	1: 	if ptr_valid(self.value) then begin ;Release
		r0 = *self.value
		p0 = self -> GetPosition(r0[0],r0[1])
		x0 = min([p0[0],p1[0]],max=x1)
		y0 = min([p0[1],p1[1]],max=y1)
		if (x0 ne x1) and (y0 ne y1) then $
			Self -> Set, ZoomWindow=[x0,y0,x1,y1], /Refresh, /erase
		ptr_free, self.value
		endif
	endcase
	ev.id=self.id & RETURN, ev
END

FUNCTION PMI__Module__StandardDraw::Scroll, ev, dim

	if ev.type eq 6 then return, self->KeyBoardNavigator(ev)
	if ev.type eq 5 then return, 0B

	case ev.type of
	0: self.value = ptr_new(1B) ;Press
	1: ptr_free, self.value 	;Release
	2: begin 					;Motion
		p0 = self.CursorPos
		p1 = self -> GetPosition(ev.x,ev.y)
		self.CursorPos[0:1] = p1
		if ptr_valid(self.value) then begin
			i = p0[dim] + p1[0]-p0[0]
			self.CursorPos[dim] = i
			self -> Set, /UpdateCursorPos, /LoadData, /Refresh
		endif
		end
	endcase
	ev.id=self.id & RETURN, ev
END



FUNCTION PMI__Module__StandardDraw::Scroll_z, ev
	return, self -> Scroll(ev,2)
END
FUNCTION PMI__Module__StandardDraw::Scroll_t, ev
	return, self -> Scroll(ev,3)
END

FUNCTION PMI__Module__StandardDraw::Event, ev
	return, call_method(self.Tool,self,ev)
END
FUNCTION PMI__Module__StandardDraw__Event, ev
	widget_control, ev.handler, get_uvalue=self
	return, self->Event(ev)
END



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;                                           ;;;;;;;
;;;;;;             SET                           ;;;;;;;
;;;;;;                                           ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






PRO PMI__Module__StandardDraw::Scl
	ptr_free, self.scl
	if not ptr_valid(self.win) then return
	Self -> Get, DimScaled=d
	scl = bytarr(3,d[0],d[1])
	for i=0L,2 do begin
		im = reform((*self.win)[i,*,*])
		scl[i,*,*] = congrid(im,d[0],d[1])
	endfor
	self.scl = ptr_new(scl,/no_copy)
END
PRO PMI__Module__StandardDraw::Win
	self -> Get, DimSlice=d
	if self.d[0] eq 0 then self.d=d
	if total(self.r+self.d gt d) gt 0 then begin
		self.r = [0,0]
		self.d = d
	endif
	ptr_free, self.win
	if not ptr_valid(self.scr) then return
	win = (*self.scr)[*,self.r[0]:self.r[0]+self.d[0]-1,self.r[1]:self.r[1]+self.d[1]-1]
	self.win = ptr_new(win,/no_copy)
END
PRO PMI__Module__StandardDraw::Scr
	ptr_free, self.scr
	if ptr_valid(self.ind) then begin
		d = size(*self.raw,/dimensions)
		scr = reform(*self.img,3,d[0]*d[1])
		scr[*,*self.ind] = Image24bit((*self.dat)[*self.ind],Self.Region->Clr(/R),Self.Region->Clr(/G),Self.Region->Clr(/B))
		scr = reform(scr,3,d[0],d[1],/overwrite)
		self.scr = ptr_new(scr,/no_copy)
		return
	endif
	if ptr_valid(self.img) then self.scr = ptr_new(*self.img)
END
PRO PMI__Module__StandardDraw::Ind
	ptr_free, self.ind
	if not ptr_valid(self.bin) then return
	ind = where(*self.bin eq 1,cnt)
	if cnt gt 0 then self.ind=ptr_new(ind,/no_copy)
END
PRO PMI__Module__StandardDraw::Bin, Bin=Bin
	ptr_free, self.bin
	if not obj_valid(Self.Region) then return
	if not obj_valid(Self.Series) then return
;	if n_elements(Bin) eq 0 then bin = Self.Region -> Read(Self.Stdy->DataPath(),z=Self.Series->z(Self.CursorPos[2]),t=Self.Series->t(Self.CursorPos[3]))
	if n_elements(Bin) eq 0 then bin = Self.Region -> Read(Self.Stdy->DataPath(),Self.CursorPos[2],Self.CursorPos[3])
	self.bin = ptr_new(bin,/no_copy)
END
PRO PMI__Module__StandardDraw::Img
	ptr_free, self.img
	if not obj_valid(Self.Series) then return
	img = Image24bit(*self.dat,Self.Series->Clr(/R),Self.Series->Clr(/G),Self.Series->Clr(/B))
	self.img = ptr_new(img,/no_copy)
END
PRO PMI__Module__StandardDraw::Dat
	ptr_free, self.dat
	if not obj_valid(Self.Series) then return
	dat = bytscl(*self.raw,min=Self.Series->trim(0),max=Self.Series->trim(1))
	self.dat = ptr_new(dat,/no_copy)
END
PRO PMI__Module__StandardDraw::Raw
	ptr_free, self.raw
	if not obj_valid(Self.Series) then return
;	raw = Self.Series->Read(Self.Stdy->DataPath(),z=Self.Series->z(Self.CursorPos[2]),t=Self.Series->t(Self.CursorPos[3]))
	raw = Self.Series->Read(Self.Stdy->DataPath(),Self.CursorPos[2],Self.CursorPos[3])
	self.raw = ptr_new(raw,/no_copy)
END
PRO PMI__Module__StandardDraw::SetRegionClr, R, G, B
	self.Region -> clr, R,G,B
	self.Stdy -> saved, 0
	Self -> Set, /Image, /Refresh
END
PRO PMI__Module__StandardDraw::SetSeriesClr, R, G, B
	self.Series -> clr, R,G,B
	self.Stdy -> saved, 0
	self -> Img
	Self -> Set, /Image, /Refresh
END
PRO PMI__Module__StandardDraw::SetSeriesTrim, value, i
	self.Series -> Trim, value, i
	self.Stdy -> saved, 0
	self -> Dat
	self -> Img
	Self -> Set, /Image, /Refresh
END



PRO PMI__Module__StandardDraw::SetRegion, Bin, new=new

	if not keyword_set(new) then $
		case self.Mode of
		'OR':Bin = *self.bin or Bin
		'AND':Bin = *self.bin and Bin
		'AND NOT':Bin = *self.bin and not Bin
		'CLEAR':
		'NEW':Self.Region = Self.Stdy -> New('REGION', $
			Domain 	= self.Series->dom(), $
			Color 	= self.Series->Clr(SAT='R'))
		endcase
	self.Region -> Write, self.Stdy->DataPath(), Bin, self.CursorPos[2], self.CursorPos[3]
	self.Stdy -> Saved, 0
	self -> Bin, Bin = Bin
	self -> Ind
	self -> Set, /Image, /Refresh
END





PRO PMI__Module__StandardDraw::Set, $
Stdy = Stdy, $
Series = Series, $
Region = Region, $
CursorPos = CursorPos, $
UpdateCursorPos = UpdateCursorPos, $
Refresh = Refresh, $
Erase = Erase, $
LoadRegion = LoadRegion, $
LoadData = LoadData, $
Image = Image, $
xsize = xsize, $
ysize = ysize, $
win = win, $
ZoomWindow = ZoomWindow, $
ResetZoom = ResetZoom, $
FOCUS = focus, $
TOOL=tool, $
MODE=mode, $
PlotLine = PlotLine


	if n_elements(mode) ne 0 then self.mode=mode
	if n_elements(tool) ne 0 then begin
		self.tool=tool
		ptr_free, self.value
	endif
	if n_elements(Stdy) ne 0 then if obj_valid(Stdy) then Self.Stdy = Stdy else self.Stdy = Obj_new()
	if n_elements(Series) ne 0 then begin
		if obj_valid(Series) then Self.Series = Series else self.Series = Obj_new()
		Self -> Set, /UpdateCursorPos
	endif
	if n_elements(Region) ne 0 then if obj_valid(Region) then Self.Region = Region else self.Region = Obj_new()

	if keyword_set(UpdateCursorPos) then begin
		if obj_valid(Self.Series) then begin
			d = Self.Series->d()
			i = where(self.CursorPos ge d,cnt)
			if cnt gt 0 then self.CursorPos[i] = d[i]-1
			i = where(self.CursorPos lt 0,cnt)
			if cnt gt 0 then self.CursorPos[i] = 0
		endif
	endif
	if n_elements(CursorPos) ne 0 then begin
		self.CursorPos = CursorPos
		Self -> Set, /UpdateCursorPos
	endif
	if n_elements(xsize) ne 0 or n_elements(ysize) ne 0 then begin
		widget_control, self.id, scr_xsize=xsize, scr_ysize=ysize
		Self -> Scl
	endif
	if n_elements(ZoomWindow) ne 0 then begin
		x0 = ZoomWindow[0]
		y0 = ZoomWindow[1]
		x1 = ZoomWindow[2]
		y1 = ZoomWindow[3]
		if (x1 eq x0) or (y1 eq y0) then self->SET, /RESETZOOM $
		else begin
	 		self.r = [x0,y0]
	 		self.d = 1+[x1-x0,y1-y0]
		endelse
		self -> Win
		self -> Scl
	endif
	if keyword_set(ResetZoom) then begin
		Self -> Get, DimSlice=d
		self.r = [0,0]
	 	self.d = d
	endif
	if keyword_set(Image) then begin
		Self -> Scr
		Self -> Win
		Self -> Scl
	endif
	if keyword_set(LoadRegion) then begin
		Self -> Bin
		Self -> Ind
		Self -> Set, /Image
	endif
	if keyword_set(LoadData) then begin
		Self -> Raw
		Self -> Dat
		Self -> Img
		Self -> Set, /LoadRegion
	endif
	if keyword_set(Win) then begin
		widget_control, self.id, get_value = win
		wset, win
	endif
	if keyword_set(erase) then begin
		Self -> Set, /Win
		erase
	endif
	if keyword_set(Refresh) then begin
		self -> Set, /Win
		if ptr_valid(self.scl) then begin
			self->Get, ImagePos=r
			tv, *self.scl, /true, r[0], r[1]
		endif else erase
	endif
	IF keyword_set(Focus) THEN BEGIN
		self->SET, /WIN
	;	loadct, 13
		plots, [0,0,1,1,0],[0,1,1,0,0],/normal, color=255, thick=10
	ENDIF

	IF n_elements(PlotLine) NE 0 THEN BEGIN
	;PlotLine = [a,b,c] are the coordinates of a line ax+by+c=0,
	;where (x,y) is the DICOM coordinate system with units of pixelsizes:
	;origin: top left, x=horizontal (L>R), y=vertical (T>B).

		IF norm(Plotline) EQ 0 THEN RETURN

		;Convert to IDL coordinate system (origin: bottom left, x:L>R, y:B>T)
		a = PlotLine[0]
		b = -PlotLine[1]
		c = PlotLine[1]*self.d[1] + PlotLine[2]

		edges = [self.r[0], self.r[0]+self.d[0], self.r[1], self.r[1]+self.d[1]]

		y0 = -(c+a*edges[0])/b	;Intersection with left edge x=self.r[0]
		y1 = -(c+a*edges[1])/b	;Intersection with right edge x=self.r[0]+self.d[0]
		x0 = -(c+b*edges[2])/a	;Intersection with bottom edge y=self.r[1]
		x1 = -(c+b*edges[3])/a	;Intersection with top edge y=self.r[1]+self.d[1]

		Intersection = [$
			(y0 LT self.r[1]+self.d[1]) and (y0 GT self.r[1]),$
			(y1 LT self.r[1]+self.d[1]) and (y1 GT self.r[1]),$
			(x0 LT self.r[0]+self.d[0]) and (x0 GT self.r[0]),$
			(x1 LT self.r[0]+self.d[0]) and (x1 GT self.r[0])]

		i = where(Intersection eq 1, cnt)
		IF cnt EQ 2 THEN BEGIN
			CASE i[0] OF
				0:i0 = [edges[0],y0]
				1:i0 = [edges[1],y1]
				2:i0 = [x0,edges[2]]
				3:i0 = [x1,edges[3]]
			ENDCASE
			CASE i[1] OF
				0:i1 = [edges[0],y0]
				1:i1 = [edges[1],y1]
				2:i1 = [x0,edges[2]]
				3:i1 = [x1,edges[3]]
			ENDCASE
			Self->GET, scale=scl, imagepos=r
			i0 = r + scl*(i0-self.r)
			i1 = r + scl*(i1-self.r)
			Self->SET, /WIN
			plots, [i0[0],i1[0]], [i0[1],i1[1]], /device
		ENDIF

	ENDIF
END








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;                                           ;;;;;;;
;;;;;;             GET                           ;;;;;;;
;;;;;;                                           ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;










FUNCTION PMI__Module__StandardDraw::GetPosition, x, y, scr_r=scr_r
	Self -> Get, Scale=s, ImagePos=r, DimWindow=d
	x = floor((x-r[0])/s)
	y = floor((y-r[1])/s)
	if x gt d[0]-1 then x=d[0]-1
	if y gt d[1]-1 then y=d[1]-1
	if x lt 0 then x=0
	if y lt 0 then y=0
	scr_r = r + s * ([x,y] + 0.5*[1,1])
	return, self.r + [x,y]
END

FUNCTION PMI__Module__StandardDraw::ScreenPosition, p
	Self -> Get, Scale=s, ImagePos=r
;	scr_r = r + s * (p - self.r + 0.5*[1,1])
	scr_r = r + s * (p - self.r)
	return, scr_r
END





PRO PMI__Module__StandardDraw::Get $
, 	id = id $
, 	bin = bin $
, 	CursorPos = CursorPos $
, 	Stdy = Stdy $
, 	Series = Series $
, 	Region = Region $
,	PixelValue = PixelValue $
,	DimDisplay = DimDisplay $
,	DimSlice = DimSlice $
,	DimWindow = DimWindow $
,	DimScaled = DimScaled $
,	Scale = Scale $
,	ImagePos = ImagePos $
,	TOOL=tool $
,	MODE=mode $
,	Raw=Raw


	if arg_present(mode) ne 0 then mode=self.mode
	if arg_present(tool) ne 0 then tool=self.tool
	if arg_present(id) then id = self.id
	if arg_present(bin) then bin = *self.bin
	if arg_present(CursorPos) then CursorPos = self.CursorPos
	if arg_present(Series) then Series = Self.Series
	if arg_present(Region) then Region = Self.Region
	if arg_present(Stdy) then Stdy = Self.Stdy
	if arg_present(raw) then if ptr_valid(self.raw) then raw = *self.raw

	if arg_present(PixelValue) then begin
		PixelValue = dblarr(6)
		PixelValue[0:1] = self.CursorPos[0:1]
		if obj_valid(Self.Series) then begin
			PixelValue[2] = Self.Series -> z(self.CursorPos[2])
			PixelValue[3] = Self.Series -> t(self.CursorPos[3])
			PixelValue[4] = Self.Series -> t(0)
			PixelValue[5] = (*self.raw)(self.CursorPos[0],self.CursorPos[1])
		endif
	endif

	if arg_present(DimDisplay) then begin
		g = widget_info(self.id,/geometry)
		DimDisplay = [g.scr_xsize,g.scr_ysize]
	endif
	if arg_present(DimSlice) then begin
		if obj_valid(Self.Series) then begin
			d = Self.Series->d()
			DimSlice = d[0:1]
		endif else self->Get, DimDisplay=DimSlice
	endif
	if arg_present(DimWindow) then begin
		DimWindow = self.d
		if DimWindow[0] eq 0 then self->Get, DimSlice=DimWindow
	endif
	if arg_present(Scale) then begin
		Self -> Get, DimDisplay = dd, DimWindow = dw
		Scale = min(dd/dw)
	endif
	if arg_present(DimScaled) then begin
		Self -> Get, Scale = s, DimWindow=dw
		DimScaled = floor(dw*s)
	endif
	if arg_present(ImagePos) then begin
		Self -> Get, DimDisplay = dd, DimWindow = dw, DimScaled = ds
		s = min(dd/dw,i)
		ImagePos = [0L,0L]
		ImagePos[1-i] = (dd[1-i]-ds[1-i])/2
	endif

END






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;                                           ;;;;;;;
;;;;;;             DEFINE                        ;;;;;;;
;;;;;;                                           ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






PRO PMI__Module__StandardDraw::Cleanup
	ptr_free, self.bin, self.ind, self.raw, self.dat, self.img, self.scr, self.win, self.scl, self.value
END
FUNCTION PMI__Module__StandardDraw::Init, parent, CursorPos=CursorPos

	if n_elements(CursorPos) ne 0 then self.CursorPos=CursorPos

	self.id	= widget_draw(parent $
	,	/retain $
	,	event_func = 'PMI__Module__StandardDraw__Event' $
	,	/button_events $
	,	/motion_events $
	,	/keyboard_events )

	self.Tool='Scroll_z'
	self.Mode='NEW'

	widget_control, self.id, set_uvalue = self

	return, 1B
END
PRO PMI__Module__StandardDraw__Define

	struct = {PMI__Module__StandardDraw 	$
	,	id:0L $
	,	Stdy	:Obj_new() $
	,	Series	:Obj_new() $
	,	Region	:Obj_new() $
	,	CursorPos:lonarr(4) $
	,	bin		:ptr_new()		$	;8bit ROI data
	,	ind		:ptr_new()		$	;ROI indices
	,	raw		:ptr_new()		$	;Raw data
	,	dat		:ptr_new()		$	;8bit Image
	,	img		:ptr_new()		$	;24bit Image
	,	scr		:ptr_new()		$	;24bit Displayed image
	,	win		:ptr_new()		$	;24bit image window
	,	scl		:ptr_new()		$	;24bit Scaled image window
	,	r		:lonarr(2)		$ 	;lower left coordinates of the image on display
	,	d		:lonarr(2)		$ 	;dimensions of the image on display
	,	Value: ptr_new() $
	,	Tool:'' $
	,	Mode:'' }

END