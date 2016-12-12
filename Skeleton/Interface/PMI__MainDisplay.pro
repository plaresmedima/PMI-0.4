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


pro PMI__MainDisplay__Cleanup, id

	PMI__INFO, id, STATE=s
	obj_destroy, s
end



pro PMI__MainDisplay__Event, ev

	PMI__Info, ev.top, Display=Display

	widget_control, ev.id, update=0
	Display -> Set, /PMI__RESIZE
	widget_control, ev.id, update=1
end



pro PMI__MainDisplay

	PMI__Display__Welcome__Define

  	xsize = 638
  	ysize = 648

	Base = widget_base($
		title 			= 'Platform for Research in Medical Imaging - Version 0.4',$
		uvalue 			= obj_new('state')	,$
		column 			= 1 				,$
	 	tlb_size_events	= 1					,$
		xoffset 		= 200 				,$
	 	yoffset			= 50				,$
  		mbar 			= MenuBar			,$
  		xsize			= xsize 			,$
  		ysize			= ysize 			)

	PMI__Menu, MenuBar

	Window = widget_base(Base,/column)

 	widget_control, Base, /realize
	Display = obj_new('PMI__Display__Welcome',Window,xsize=xsize,ysize=ysize)

	PMI__Control, Base, /MenuRefresh

	xmanager, $
	 	'PMI__MainDisplay', $
	 	Base, event_handler = 'PMI__MainDisplay__Event', $
		cleanup = 'PMI__MainDisplay__Cleanup'

end