

pro PMI__Button__Event__HelloWorld, ev	;The procedure to be called after button press

	ok = dialog_message(/information,'Hello World')

end


PRO PMI__Button__Control__HelloWorld, id, v ;Makes the button sensitive as soon as a series is on display

END



FUNCTION PMI__Button__HelloWorld, $ ;Define the button
	parent,$
	separator=separator,$
	value=value

	IF n_elements(value) EQ 0 THEN value='Hello World Default' ;Default value for the button label

	RETURN, widget_button(parent,$
	 	value = value,$
	 	event_pro = 'PMI__Button__Event__HelloWorld', $		;The event procedure to be called upon button press
	  	pro_set_value =  'PMI__Button__Control__HelloWorld', $ 	;The control procedure that defines the sensitivity of the button
		separator =	separator) ;If this keyword is set, a line is drawn above the button in the menu
END
