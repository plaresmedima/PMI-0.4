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


PRO PMI__Display__ViewHistogram::TextStats, stats, hist=hist

	stats = strarr(2,13)

	stats[0,*] = [$
	'Nr. of values  ',$
	'Mean           ',$
	'Stdev          ',$
	'Minimum        ',$
	'Maximum        ',$
	'1% percentile  ',$
	'10% percentile ',$
	'25% percentile ',$
	'Median         ',$
	'75% percentile ',$
	'90% percentile ',$
	'99% percentile ',$
	'Mode           ']

	stats[1,*] = [$
	strcompress(self.nvals,/remove_all),$
	strcompress(self.avr,/remove_all),$
	strcompress(self.sdev,/remove_all),$
	strcompress(self.minv,/remove_all),$
	strcompress(self.maxv,/remove_all),$
	strcompress(self.perc[0],/remove_all),$
	strcompress(self.perc[1],/remove_all),$
	strcompress(self.perc[2],/remove_all),$
	strcompress(self.perc[3],/remove_all),$
	strcompress(self.perc[4],/remove_all),$
	strcompress(self.perc[5],/remove_all),$
	strcompress(self.perc[6],/remove_all),$
	strcompress(self.mode,/remove_all)]

	if arg_present(hist) then begin
		hist = strarr(2,1+n_elements(*self.xhist))
		hist[*,0] = ['X-values','Frequency (%)']
		hist[0,1:*] = strcompress(*self.xhist,/remove_all)
		hist[1,1:*] = strcompress(*self.yhist,/remove_all)
	endif
END

PRO PMI__Display__ViewHistogram::Event, ev

	case ev.id of

		self.minx: Self->Histogram
		self.maxx: Self->Histogram
		self.nbins: Self->Histogram
		self.type: Self->Histogram

		self.title: Self->Plot
		self.xtitle: Self->Plot
		self.thick: Self->Plot
		self.size: Self->Plot

		self.save:begin
			PMI__Info, ev.top, Stdy=Stdy
			Path = Stdy->DataPath() + 'ROI statistics'
			file_mkdir, Path
			Filename = Path + '\' + Self.Series + '_' + Self.Region + '_Histogram'
			write_tiff, Filename + '.tif', reverse(tvrd(/true),3)
			Self -> TextStats, stats, hist=hist
			PMI__Write_csv, Filename +'_plot.csv', hist
			PMI__Write_csv, Filename +'_stats.csv', stats
			PMI__Write_csv, Filename +'_data.csv', *self.data
			end

		self.saveas:begin
			PMI__Info, ev.top, State=State
			Filename = Self.Series + '_' + Self.Region + '_Histogram'
			if not State -> get_file(file, file=cleanstr(Filename), title='Save plot as..', filter='.tif') then return
			Write_tiff, file, reverse(tvrd(),2)
			Filename = strmid(file,0,strlen(file)-4)
			Self -> TextStats, stats, hist=hist
			PMI__Write_csv, Filename +'_plot.csv', hist
			PMI__Write_csv, Filename +'_stats.csv', stats
			PMI__Write_csv, Filename +'_data.csv', *self.data
			end

		self.close:begin
			Menu = widget_info(widget_info(ev.top,/child),/all_children)
			for i=0L,n_elements(Menu)-1 do widget_control, Menu[i], /sensitive
			PMI__Control, ev.top, Viewer = 'PMI__DISPLAY__2DVIEW'
			PMI__Control, ev.top, /refresh
			end
	endcase
END
PRO PMI__Display__Event__ViewHistogram, ev
	widget_control, ev.handler, get_uvalue=self
	Self -> Event, ev
END













PRO PMI__Display__ViewHistogram::Plot

	widget_control, self.size, get_value=v & charsize = v[widget_info(self.size,/droplist_select)]
	widget_control, self.thick, get_value=v & charthick = v[widget_info(self.thick,/droplist_select)]
	widget_control, self.title, get_value=title
	widget_control, self.xtitle, get_value=xtitle

	widget_control, self.plot, get_value = win
	wset, win

	top=0.875 & dy=0.05 & x0=0.525

	if not ptr_valid(self.xhist) then begin

		erase, 255
		xyouts,x0,top-0*dy,'INVALID HISTOGRAM SETTINGS',color=12*16,/normal,charsize=charsize,charthick=charthick
		xyouts,x0,top-2*dy,'>> Upper value must be larger than Lower value',color=0,/normal,charsize=charsize,charthick=charthick
		xyouts,x0,top-3*dy,'>> The number of bins must be larger than 1' ,color=0,/normal,charsize=charsize,charthick=charthick

	endif else begin

		if widget_info(self.type,/droplist_select) eq 0 then begin
	 		plot, *self.xhist, *self.yhist, position=[0.1,0.2,0.5,0.9]  $
 			,	Title = title, 	xtitle = xtitle, ytitle='Frequency (%)' $
			, 	/xstyle, /ystyle, background=255, color=0, psym=10 $
			, 	charsize=charsize, charthick=charthick, xthick=charthick, ythick=charthick, thick=charthick
		endif else begin
 			plot, 	*self.xhist, *self.yhist, position=[0.1,0.2,0.5,0.9]  $
 			,	Title = title, 	xtitle = xtitle, ytitle='Percentile (%)' $
			, 	/xstyle, /ystyle, background=255, color=0, linestyle=0 $
			, 	charsize=charsize, charthick=charthick, xthick=charthick, ythick=charthick, thick=charthick
		endelse

		xyouts,x0,top-0*dy,'SERIES: ' + self.Series 	,color=0,/normal,charsize=charsize,charthick=charthick
		xyouts,x0,top-1*dy,'REGION: ' + self.Region 	,color=0,/normal,charsize=charsize,charthick=charthick

		Self -> TextStats, stats
		for i=0L,n_elements(stats[0,*])-1 do $
			xyouts,x0,top-(i+3)*dy, stats[0,i] + stats[1,i], color=0,/normal,charsize=charsize,charthick=charthick

	endelse

	Menu = widget_info(widget_info(tlb(self.id),/child),/all_children)
	for i=0L,n_elements(Menu)-1 do widget_control, Menu[i], sensitive=0

END





PRO PMI__Display__ViewHistogram::Histogram

	widget_control, self.maxx, get_value=v & v1=float(v[0])
	widget_control, self.minx, get_value=v & v0=float(v[0])
	widget_control, self.nbins, get_value=v & Nbins = Long(v[0])
	type = widget_info(self.type,/droplist_select)

	ptr_free, self.yhist, self.xhist
	if (v1 gt v0) and (nbins gt 0) then begin
		binsize = (v1-v0)/nbins
		yhist = histogram(*self.data,min=v0,max=v1,binsize=binsize)
		yhist = 100.0*yhist/total(yhist)
		nhist = n_elements(yhist)
		self.xhist  = ptr_new(binsize*findgen(nhist) + v0 + binsize/2)
		ymax 		= max(yhist,imax)
		self.mode 	= (*self.xhist)[imax]
		if type eq 1 then for i=1L,nhist-1 do yhist[i]=yhist[i]+yhist[i-1]
		self.yhist = ptr_new(yhist)
	endif
	Self -> Plot
END






PRO PMI__Display__ViewHistogram::Size, xsize, ysize

	widget_control, self.id, xsize=xsize, ysize=ysize
	xs = floor((xsize - 500)/6E)
	if xs lt 10 then xs=10
	widget_control, self.title, xsize=xs
	widget_control, self.xtitle, xsize=xs
	widget_control, self.plot, scr_xsize=xsize, scr_ysize=ysize-115

END
PRO PMI__Display__ViewHistogram::GET, CursorPos=CursorPos

	if arg_present(CursorPos) then CursorPos=self.CursorPos
END
PRO PMI__Display__ViewHistogram::SET, PMI__REFRESH=pmi__refresh, PMI__RESIZE=pmi_resize

	if keyword_set(pmi_resize) then begin
		gpmi = widget_info(/geometry, tlb(self.id))
		self -> Size, gpmi.xsize, gpmi.ysize
		self -> Plot
	endif
END



PRO PMI__Display__ViewHistogram::Cleanup

	ptr_free, self.yhist, self.xhist, self.data
	widget_control, self.id, /destroy
	loadct, 0
END

FUNCTION PMI__Display__ViewHistogram::Init, parent, CursorPos, xsize=xsize,ysize=ysize, Data=v, Region=Region, Series=Series

	loadct, 12

	;INITIALIZE

	self.CursorPos=CursorPos
	self.nvals	= n_elements(v)
	self.perc 	= Percentiles(v,[1,10,25,50,75,90,99])
	self.avr 	= mean(v)
	self.sdev 	= stddev(v)
	self.minv 	= min(v)
	self.maxv 	= max(v)
	self.data 	= ptr_new(v,/no_copy)
	self.Region = cleanstr(Region)
	self.Series = cleanstr(Series)


	;BUILD DISPLAY

	self.id = widget_base(parent,/column,event_pro='PMI__Display__Event__ViewHistogram')

	Controls = widget_base(self.id,/row,ysize=100,uname='Controls')
	self.Plot = widget_draw(self.id,/retain)

	settings = widget_base(Controls, /column, /frame)

		xlabel=80 & xtext=8
		id = widget_base(settings,/row)
			label = widget_label(id, xsize=xlabel, value='Upper value')
			self.maxx = widget_text(id, /editable, /all_events, xsize=xtext, value=strcompress(self.maxv,/remove_all) )
		id = widget_base(settings,/row)
			label = widget_label(id, xsize=xlabel, value='Lower value')
			self.minx = widget_text(id, /editable, /all_events, xsize=xtext, value=strcompress(self.minv,/remove_all) )
		id = widget_base(settings,/row)
			label = widget_label(id, xsize=xlabel, value='Number of bins')
		 	self.nbins = widget_text(id, /editable, /all_events, xsize=xtext, value=strcompress(100L,/remove_all))

	graphics = widget_base(Controls, /column, /frame)

		xlabel=80 & xlist=80
		id = widget_base(graphics,/row)
			label = widget_label(id, xsize=xlabel, value='Text size')
  			self.size = widget_droplist(id, xsize=xlist, value = ['1.0','1.5','2.0','2.5','3.0'] )
  			widget_control, self.size, set_droplist_select=2
		id = widget_base(graphics,/row)
			label = widget_label(id, xsize=xlabel, value='Line thickness')
  			self.thick = widget_droplist(id, xsize=xlist, value = ['1.0','1.5','2.0','2.5','3.0','3.5','4.0'] )
  			widget_control, self.thick, set_droplist_select=2
		id = widget_base(graphics,/row)
			label = widget_label(id, xsize=xlabel, value='View')
  			self.type = widget_droplist(id, xsize=xlist, value = ['Histogram','Cumulative'] )
  			widget_control, self.type, set_droplist_select=0

	labels = widget_base(Controls, /column, /frame)

		xlabel=60
		id = widget_base(labels,/row)
			label = widget_label(id, xsize=xlabel, value='Plot title')
			self.title = widget_text(id, /editable, /all_events, value='Histogram')
		id = widget_base(labels,/row)
			label = widget_label(id, xsize=xlabel, value='X-axis title')
			self.xtitle = widget_text(id, /editable, /all_events, value='ROI Values')

	buttons = widget_base(Controls, /column, /frame)

		ybttn=27
		self.save = widget_button(buttons, ysize=ybttn, value='Save'	,uname='Save')
		self.saveas = widget_button(buttons, ysize=ybttn, value='Save As',uname='Save As')
		self.close = widget_button(buttons, ysize=ybttn, value='Close'	,uname='Close')

	Self -> Size, xsize, ysize
	Self -> Histogram

	widget_control, self.id, set_uvalue = self

	return, 1
END



PRO PMI__Display__ViewHistogram__Define

	struct = {PMI__Display__ViewHistogram 	$
		;WIDGET_ID's
	,	id		: 0L $
	,	title	: 0L $
	,	xtitle	: 0L $
	,	size	: 0L $
	,	thick	: 0L $
	,	minx	: 0L $
	,	maxx	: 0L $
	,	nbins	: 0L $
	,	save	: 0L $
	,	saveas	: 0L $
	,	close	: 0L $
	,	plot	: 0L $
	,	type	: 0L $
		;VALUES
	,	CursorPos	:lonarr(4) $
	,	Data	: ptr_new() $
	,	Series	:'' $
	,	Region	:'' $
	,	yhist 	: ptr_new() $
	,	xhist   : ptr_new() $
	,	nvals	: 0L $
	,	perc 	: fltarr(7) $
	,	avr 	: 0E $
	,	sdev 	: 0E $
	,	mode 	: 0E $
	,	minv 	: 0E $
	,	maxv 	: 0E $
	}
END


