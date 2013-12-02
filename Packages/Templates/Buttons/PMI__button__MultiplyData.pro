

pro PMI__Button__Event__MultiplyData, ev	;The procedure to be called after button press

	PMI__info, ev.top, Stdy=Stdy, Series=Series

  	Smth = Stdy -> New('SERIES', Default	= Series )     ; Name of the new series 'Smth'

	data = Series -> Read(Stdy->DataPath())
	Smth -> Write, Stdy->DataPath(), data*5

	PMI__control, ev.top, /refresh ;Refresh the PMI display
end


PRO PMI__Button__Control__MultiplyData, id, v ;Makes the button sensitive as soon as a series is on display

	PMI__Info, tlb(id), Series=Series	;Get the series currently on display
    widget_control, id, sensitive=obj_valid(Series) ;Button is sensitive iff the series list is not <empty>
END



FUNCTION PMI__Button__MultiplyData, $ ;Define the button
	parent,$
	separator=separator,$
	value=value

	IF n_elements(value) EQ 0 THEN value='Multiply default' ;Default value for the button label

	RETURN, widget_button(parent,$
	 	value = value,$
	 	event_pro = 'PMI__Button__Event__MultiplyData', $		;The event procedure to be called upon button press
	  	pro_set_value =  'PMI__Button__Control__MultiplyData', $ 	;The control procedure that defines the sensitivity of the button
		separator =	separator) ;If this keyword is set, a line is drawn above the button in the menu
END
