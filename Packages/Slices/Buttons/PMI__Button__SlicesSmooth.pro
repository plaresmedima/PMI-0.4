;DESCRIPTION:
;	Applies uniform smoothing with a user-defined smoothing window
;	to the series currently on display
;	Uses the standard IDL function SMOOTH()

;OUTPUT:
;	Smoothed data are saved in a new series.
;	Default name of the new series is that of the original series,
;		with a suffix [Smth xx], where xx ist the window used for smoothing.

;CALLING SEQUENCE: PMI__Button__SlicesSmooth(parent,VALUE=buttonlabel, /SEPARATOR)

;WRITTEN BY: Steven Sourbron

;INTRODUCED: In version PMI 0.2

;UPDATED: July 2009 (upgrade to PMI 0.4)

;LOCATION: Steven_Sourbron>MyButtons>Slices

;REQUIRED LIBRARIES/PROCEDURES: None.


;
;
;    Copyright (C) 2009 Steven Sourbron
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
;    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
;
;
;



pro PMI__Button__Event__SlicesSmooth, ev	;The procedure to be called after button press



;Get the Input for the calculation

	PMI__info, ev.top, Status=Status, Stdy=Stdy, Series=Series

		;'Series' is the series currently on display
		;'Stdy' is the Study of which it is a part
		;'Status' is the widget id of the status bar
		;	It will be used to give the user an update on the progress of the calculation




;Get the user input with PMI__FORM(): in this case the size of the smoothing window

;	PMI__FORM() is the standard PMI pop-up for user input
;	Its source code can be found in Source>Libraries>Widgets
;	Any number of items can be requested from the user.
;	In this case, only one item is required (the smoothing window).
;		It is an item of type 'VALUE' (alternatives are LIST or DROPLIST)
;		It can be accessed with the tag 'win' (this is an optional argument, default tag is 'item0')
;		It is labeled 'Smoothing window' in the form
;		It a default value of 3

	v = PMI__Form(ev.top, Title='Signal smoothing', $
		[ ptr_new({Type:'VALUE'	,Tag:'win',Label: 'Smoothing window', Value:3L}) ] $
		)

	if v.cancel then return ; if the users hits cancel, then exit the event

		;v.win is now the size of the smoothing window defined by the user

	win = 2*(v.win/2) + 1	;if the user has selected an even number, then make it odd -- required by SMOOTH()
	d = Series -> d()		;Get the x,y,z,t dimensions of the Series

  	Smth = Stdy -> New('SERIES' $		;Create a new series object 'Smth' in the same Study, which will store the output
  	,	Default	= Series $				;'Smth' has the same dimensions, color window, slice locations etc as the original series
  	, 	Name	= Series->name() +'[Smth ' + strcompress(win,/remove_all) + ' ]' )     ; Name of the new series 'Smth'


	;Now comes the loop over all slices
	;Each slice in the orignal Series is loaded, smoothed, and the smoothed slice is written in the new Series 'Smth'
	;For each iteration, the status bar is refreshed to show the progress of the calculation

	FOR k=0L,d[2]*d[3]-1 DO BEGIN
		PMI__Message, status, 'Smoothing '+Series->name(), k/(d[2]*d[3]-1E)	;Update the status bar, displaying a message and a counter
		im = Series -> Read(Stdy->DataPath(),k)		;load image 'k' of Series
		im = smooth(im,win,/edge_truncate)		;Smooth the image
		Smth -> Write, Stdy->DataPath(), im, k	;Write the smoothed image in the new series object 'Smth'
	ENDFOR

	PMI__control, ev.top, /refresh ;Refresh the PMI display
end






PRO PMI__Button__Control__SlicesSmooth, id, v ;Makes the button sensitive as soon as a series is on display

	PMI__Info, tlb(id), Series=Series	;Get the series currently on display
    widget_control, id, sensitive=obj_valid(Series) ;Button is sensitive iff the series list is not <empty>
END





FUNCTION PMI__Button__SlicesSmooth, $ ;Define the button
	parent,$
	separator=separator,$
	value=value

	IF n_elements(value) EQ 0 THEN value='Uniform smoothing' ;Default value for the button label

	RETURN, widget_button(parent,$
	 	value = value,$
	 	event_pro = 'PMI__Button__Event__SlicesSmooth', $		;The event procedure to be called upon button press
	  	pro_set_value =  'PMI__Button__Control__SlicesSmooth', $ 	;The control procedure that defines the sensitivity of the button
		separator =	separator) ;If this keyword is set, a line is drawn above the button in the menu
END
