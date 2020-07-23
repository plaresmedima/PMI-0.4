


PRO PMI__Button__Event__iBEAt_BatchProcessSingle, ev

	msg = 'Not yet implemented'
	ok = dialog_message(msg,/information)

END



PRO PMI__Button__Control__iBEAt_BatchProcessSingle, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	widget_control, id, sensitive = obj_valid(Stdy)

END


FUNCTION PMI__Button__iBEAt_BatchProcessSingle, parent, value=value, separator=separator

	IF n_elements(value) EQ 0 THEN value = 'Batch process - single case'

	RETURN, widget_button(parent, value=value, separator=separator, $
		event_pro = 'PMI__Button__Event__iBEAt_BatchProcessSingle', $
		pro_set_value = 'PMI__Button__Control__iBEAt_BatchProcessSingle' )

END
