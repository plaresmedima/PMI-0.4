

pro PMI__Button__Event__SimpleSmooth, ev	;The procedure to be called after button press

	PMI__info, ev.top, Stdy=Stdy, Series=Series

  	Smth = Stdy -> New('SERIES', Default= Series )     ; Name of the new series 'Smth'

	d = Series -> d() ;[x,y,z,t]

	FOR k=0L,d[2]*d[3]-1 DO BEGIN
		im = Series -> Read(Stdy->DataPath(),k)		;load image 'k' of Series
		im = smooth(im,5,/edge_truncate)		;Smooth the image
		Smth -> Write, Stdy->DataPath(), im, k	;Write the smoothed image in the new series object 'Smth'
	ENDFOR

	PMI__control, ev.top, /refresh ;Refresh the PMI display
end


PRO PMI__Button__Control__SimpleSmooth, id, v ;Makes the button sensitive as soon as a series is on display

	PMI__Info, tlb(id), Series=Series	;Get the series currently on display
    widget_control, id, sensitive=obj_valid(Series) ;Button is sensitive iff the series list is not <empty>
END



FUNCTION PMI__Button__SimpleSmooth, $ ;Define the button
	parent,$
	separator=separator,$
	value=value

	IF n_elements(value) EQ 0 THEN value='Simple smooth default' ;Default value for the button label

	RETURN, widget_button(parent,$
	 	value = value,$
	 	event_pro = 'PMI__Button__Event__SimpleSmooth', $		;The event procedure to be called upon button press
	  	pro_set_value =  'PMI__Button__Control__SimpleSmooth', $ 	;The control procedure that defines the sensitivity of the button
		separator =	separator) ;If this keyword is set, a line is drawn above the button in the menu
END
