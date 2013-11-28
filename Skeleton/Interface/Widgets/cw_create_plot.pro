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


pro cw_create_plot_refresh, base

	top = widget_info(base,/parent)
	draw = widget_info(base,/child)
	widget_control, top, get_uvalue = s
	widget_control, draw, get_value = win
	wset, win

	if (s.MinX ge s.MaxX) $
	or (s.MinY ge s.MaxY) then begin
		erase
		return
	endif

	loadct, 0

	plot $
	, 	s.X, s.Y $
	,	psym = s.psym $
	,	charsize=s.charsize, charthick=s.charthick, thick=s.thick $
	,	title = s.title, xtitle = s.xtitle, ytitle = s.ytitle $
	,	yrange = [s.MinY,s.MaxY], /ystyle $
	,	xrange = [s.MinX,s.MaxX], /xstyle
	if n_elements(s.curve2) eq n_elements(s.x)then $
		oplot, s.X, s.curve2,linestyle=2
end

pro cw_create_plot_event, event

	widget_control, event.top, get_uvalue = state
	widget_control, event.id, get_uvalue = uvalue

	if uvalue eq 'button' then begin
		cw_create_plot_refresh, event.handler
		filename = dialog_pickfile( $
			title = 'Save plot as' $
		,	file = 'plot_'+ cleanstr(state.title))
		if filename eq '' then return
		print, filename
		write_jpeg, filename + '.jpeg', 255-tvrd()
		PMI__WritePlot, filename+'.txt', state.X, state.Y
		return
	endif

	case uvalue of
		'MinX':state.MinX = event.value
		'MaxX':state.MaxX = event.value
		'MinY':state.MinY = event.value
		'MaxY':state.MaxY = event.value
		'title':state.title = event.value
		'xlabel':state.xtitle = event.value
		'ylabel':state.ytitle = event.value
		'psym': state.psym = event.index
		'charsize':state.charsize = float(event.value)
		'charthick':state.charthick = float(event.value)
		'thick':state.thick = float(event.value)
	endcase

	widget_control, event.top, set_uvalue = state
	cw_create_plot_refresh, event.handler

end


pro cw_create_plot $
   ,Y $
   ,xaxis = X $
   ,title = title $
   ,xtitle= xtitle $
   ,ytitle= ytitle $
   ,curve2=curve2


  if n_elements(X) 		eq 0 	then X = lindgen(n_elements(Y))
  if n_elements(title) 	eq 0 	then title = 'PLOT'
  if n_elements(xtitle) 	eq 0 	then xtitle = 'XAXIS'
  if n_elements(ytitle) 	eq 0 	then ytitle = 'YAXIS'
  IF n_elements(curve2)NE n_elements(x) THEN curve2 = 0

  state = {X:X, Y:Y $
           ,curve2:curve2 $
           ,MinX:min(X) $
           ,MaxX:Max(X) $
           ,MinY:min(Y) $
           ,maxY:max(Y) $
           ,psym:0, charsize:1.0, charthick:1.0, thick:1.0 $
           ,title:title, xtitle:xtitle, ytitle:ytitle }


  parent = widget_base(title='PLOT EDITOR',uvalue=state)

  base = widget_base(parent, /column, $
                     /base_align_left, /frame, $
                     event_pro = 'cw_create_plot_event')

  draw = widget_draw(base, xsize=550, ysize=350)
  plot_settings = widget_base(base,/row)




  data = widget_base(plot_settings, /column, /frame)

  id = cw_field(data $
                , 	/all_events, xsize = 10 $
                , 	value = strcompress(state.MaxY,/remove_all) $
                , 	uvalue = 'MaxY', title = 'Y-axis maximum')
  id = cw_field(data $
                , 	/all_events, xsize = 10 $
                , 	value = strcompress(state.MinY,/remove_all) $
                , 	uvalue = 'MinY', title = 'Y-axis minimum')
  id = cw_field(data $
                , 	/all_events, xsize = 10 $
                , 	value = strcompress(state.MaxX,/remove_all) $
                , 	uvalue = 'MaxX', title = 'X-axis maximum')
  id = cw_field(data $
                , 	/all_events, xsize = 10 $
                , 	value = strcompress(state.MinX,/remove_all) $
                , 	uvalue = 'MinX', title = 'X-axis minimum')



  graphics = widget_base(plot_settings, /column, /frame)

  id = widget_droplist(graphics $
                       ,	value 	= ['line','plus (+)','asterisk (*)','period (.)','diamond','triangle','square'] $
                       , 	uvalue 	= 'psym')
  id = cw_field(graphics, /all_events, xsize = 10 $
                ,	value	= strcompress(state.charsize,/remove_all) $
                , 	title	= 'Character size: ' $
                , 	uvalue	= 'charsize')
  id = cw_field(graphics, /all_events, xsize = 10 $
                ,	value	= strcompress(state.charthick,/remove_all) $
                , 	title	= 'Character thickness: ' $
                , 	uvalue	= 'charthick')
  id = cw_field(graphics, /all_events, xsize = 10 $
                , 	value	= strcompress(state.thick,/remove_all) $
                , 	title	= 'Line thickness: ' $
                , 	uvalue	= 'thick')
  id = cw_field(graphics, /all_events, xsize = 30 $
                ,	value	= state.title $
                , 	title	= 'Plot title:    ' $
                , 	uvalue	= 'title')
  id = cw_field(graphics, /all_events, xsize = 30 $
                , 	value	= state.ytitle $
                ,	title	= 'Y-axis label: ' $
                , 	uvalue 	= 'ylabel')
  id = cw_field(graphics, /all_events, xsize = 30 $
                , 	value	= state.xtitle $
                ,	title	='X-axis label: ' $
                , 	uvalue	= 'xlabel')


  id = widget_button(plot_settings $
                     ,	value 	= 'SAVE PLOT' $
                     ,	uvalue = 'button' )


  widget_control, parent, /realize
  cw_create_plot_refresh, base
end
