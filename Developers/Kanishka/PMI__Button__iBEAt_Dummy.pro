

PRO PMI__Button__Event__iBEAt_Dummy, ev
END


PRO PMI__Button__Control__iBEAt_Dummy, id, v

	widget_control, id, sensitive = 0
END


function PMI__Button__iBEAt_Dummy, parent, value=value, separator=separator

	if n_elements(value) eq 0 then value = 'Dummy processing'

	return, widget_button(parent, value=value, separator=separator, $
		event_pro = 'PMI__Button__Event__iBEAt_Dummy', $
		pro_set_value = 'PMI__Button__Control__iBEAt_Dummy' )

end
