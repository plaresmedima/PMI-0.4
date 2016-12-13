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


PRO PMI__Module__PlotControls::Event, ev

	case widget_info(ev.id,/uname) of

		'MinX': begin
			widget_control, ev.id, get_value=v
			Self.Plot->SET, MinX=v, /Refresh
			end
		'MinY': begin
			widget_control, ev.id, get_value=v
			Self.Plot->SET, MinY=v, /Refresh
			end
		'MaxX': begin
			widget_control, ev.id, get_value=v
			Self.Plot->SET, MaxX=v, /Refresh
			end
		'MaxY': begin
			widget_control, ev.id, get_value=v
			Self.Plot->SET, MaxY=v, /Refresh
			end
		'title': begin
			widget_control, ev.id, get_value=v
			Self.Plot->SET, title=v, /Refresh
			end
		'xtitle': begin
			widget_control, ev.id, get_value=v
			Self.Plot->SET, xtitle=v, /Refresh
			end
		'ytitle': begin
			widget_control, ev.id, get_value=v
			Self.Plot->SET, ytitle=v, /Refresh
			end

		'thick'		: Self.Plot->SET, thick = 1+ev.index/2E, /Refresh
		'charsize'	: Self.Plot->SET, charsize = 1+ev.index/2E, /Refresh
		'symsize'	: Self.Plot->SET, symsize = 1+ev.index/2E, /Refresh
		'psym'		: Self.Plot->SET, Psym = ev.index, /Refresh

		'Reset': begin
			Self.Plot->SET, /Initialize, /Refresh
			self -> Set, /Values
			end

		'Save':begin
			Self.Plot->GET, title=title, Xaxis=X, Yaxis=Y
			PMI__Info, ev.top, Stdy=Stdy
			Write_tiff, Stdy->Datapath() + title + '.tif', reverse(tvrd(),2)
			PMI__WritePlot, Stdy->Datapath() + title + '.txt', X, Y
			end

		'Save As':begin
			Self.Plot->GET, title=title, Xaxis=X, Yaxis=Y
			PMI__Info, ev.top, State=State
			if not State -> get_file(file, file=cleanstr(title), title='Save plot as..', filter='.tif') then return
			Write_tiff, file, reverse(tvrd(),2)
			PMI__WritePlot, strmid(file,0,strlen(file)-3) + 'txt', X, Y
			end

		'Close':begin
			PMI__Control, ev.top, Viewer = 'PMI__DISPLAY__2DVIEW'
			PMI__Control, ev.top, /refresh
			end
	endcase
END
PRO PMI__Module__Event__PlotControls, ev
	widget_control, ev.handler, get_uvalue=self
	Self -> Event, ev
END


PRO PMI__Module__PlotControls::SET $
, 	Plot = Plot $
,	Xsize = Xsize $
, 	Values = Values

	if n_elements(Plot) ne 0 then self.Plot=Plot

	if n_elements(Xsize) ne 0 then begin

		xs = floor((xsize - 495)/6E)
		if xs lt 10 then xs =10
		widget_control, widget_info(self.id,find_by_uname='title'), xsize=xs
		widget_control, widget_info(self.id,find_by_uname='xtitle'), xsize=xs
		widget_control, widget_info(self.id,find_by_uname='ytitle'), xsize=xs
	endif

	if keyword_set(Values) then begin

		Self.Plot->GET $
		, psym=psym, symsize=symsize $
		, charsize=charsize, thick=thick $
		, title=title, xtitle=xtitle, ytitle=ytitle $
		, MinX=MinX, MaxX=MaxX $
		, MinY=MinY, MaxY=MaxY

		widget_control, widget_info(self.id,find_by_uname='MinX')		, set_value=strcompress(MinX,/remove_all)
		widget_control, widget_info(self.id,find_by_uname='MinY')		, set_value=strcompress(MinY,/remove_all)
		widget_control, widget_info(self.id,find_by_uname='MaxX')		, set_value=strcompress(MaxX,/remove_all)
		widget_control, widget_info(self.id,find_by_uname='MaxY')		, set_value=strcompress(MaxY,/remove_all)
		widget_control, widget_info(self.id,find_by_uname='charsize')	, set_droplist_select=2*(charsize-1)
		widget_control, widget_info(self.id,find_by_uname='symsize')	, set_droplist_select=2*(symsize-1)
		widget_control, widget_info(self.id,find_by_uname='thick')		, set_droplist_select=2*(thick-1)
		widget_control, widget_info(self.id,find_by_uname='title')		, set_value=title
		widget_control, widget_info(self.id,find_by_uname='xtitle')		, set_value=xtitle
		widget_control, widget_info(self.id,find_by_uname='ytitle')		, set_value=ytitle
		widget_control, widget_info(self.id,find_by_uname='psym')		, set_droplist_select=psym
	endif

END


PRO PMI__Module__PlotControls::Cleanup
END
FUNCTION PMI__Module__PlotControls::Init, parent

	self.id = widget_base(parent,/row,ysize=130,event_pro='PMI__Module__Event__PlotControls')

	range = widget_base(self.id, /column, /frame)

		xlabel=70 & xtext=8
		id = widget_base(range,/row)
			label = widget_label(id, xsize=xlabel, value='Y-axis (Max)')
			text = widget_text(id, /editable, /all_events, xsize=xtext, uname='MaxY')
		id = widget_base(range,/row)
			label = widget_label(id, xsize=xlabel, value='Y-axis (Min)')
			text = widget_text(id, /editable, /all_events, xsize=xtext, uname='MinY')
		id = widget_base(range,/row)
			label = widget_label(id, xsize=xlabel, value='X-axis (Max)')
			text = widget_text(id, /editable, /all_events, xsize=xtext, uname='MaxX')
		id = widget_base(range,/row)
			label = widget_label(id, xsize=xlabel, value='X-axis (Min)')
			text = widget_text(id, /editable, /all_events, xsize=xtext, uname='MinX')

	graphics = widget_base(self.id, /column, /frame)

		xlabel=80 & xlist=80
		id = widget_base(graphics,/row)
			label = widget_label(id, xsize=xlabel, value='Plot symbol')
  			list = widget_droplist(id, xsize=xlist, uname='psym',value = ['line','plus (+)','asterisk (*)','period (.)','diamond','triangle','square','X'] )
		id = widget_base(graphics,/row)
			label = widget_label(id, xsize=xlabel, value='Symbol size')
  			list = widget_droplist(id, xsize=xlist, uname='symsize',value = ['1.0','1.5','2.0','2.5','3.0','3.5','4.0'] )
		id = widget_base(graphics,/row)
			label = widget_label(id, xsize=xlabel, value='Text size')
  			list = widget_droplist(id, xsize=xlist, uname='charsize',value = ['1.0','1.5','2.0','2.5','3.0'] )
		id = widget_base(graphics,/row)
			label = widget_label(id, xsize=xlabel, value='Line thickness')
  			list = widget_droplist(id, xsize=xlist, uname='thick',value = ['1.0','1.5','2.0','2.5','3.0','3.5','4.0'] )

	labels = widget_base(self.id, /column, /frame)

		xlabel=60 & xtext=30
		id = widget_base(labels,/row)
			label = widget_label(id, xsize=xlabel, value='Plot title')
			text = widget_text(id, /editable, /all_events, xsize=xtext, uname='title')
		id = widget_base(labels,/row)
			label = widget_label(id, xsize=xlabel, value='Y-axis title')
			text = widget_text(id, /editable, /all_events, xsize=xtext, uname='ytitle')
		id = widget_base(labels,/row)
			label = widget_label(id, xsize=xlabel, value='X-axis title')
			text = widget_text(id, /editable, /all_events, xsize=xtext, uname='xtitle')

	buttons = widget_base(self.id, /column, /frame)

		ybttn=27
		id = widget_button(buttons, ysize=ybttn, value='Reset'	,uname='Reset')
		id = widget_button(buttons, ysize=ybttn, value='Save'	,uname='Save')
		id = widget_button(buttons, ysize=ybttn, value='Save As',uname='Save As')
		id = widget_button(buttons, ysize=ybttn, value='Close'	,uname='Close')

	widget_control, self.id, set_uvalue = self
	return, 1B
END


PRO PMI__Module__PlotControls__Define

	struct = {PMI__Module__PlotControls 	$
	,	id:0L $
	,	Plot:obj_new() $
	}

END
