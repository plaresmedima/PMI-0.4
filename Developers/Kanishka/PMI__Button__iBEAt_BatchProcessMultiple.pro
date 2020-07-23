


PRO PMI__Button__Event__iBEAt_BatchProcessMultiple, ev

	msg = 'Not yet implemented'
	ok = dialog_message(msg,/information)

END


pro PMI__Button__Control__iBEAt_BatchProcessMultiple, id, v

	widget_control, id, sensitive = 1

;	PMI__Info, tlb(id), Stdy=Stdy
;	widget_control, id, sensitive = obj_valid(Stdy)
end


function PMI__Button__iBEAt_BatchProcessMultiple, parent, value=value, separator=separator

	if n_elements(value) eq 0 then value = 'Batch process - multiple cases'

	return, widget_button(parent, value=value, separator=separator, $
		event_pro = 'PMI__Button__Event__iBEAt_BatchProcessMultiple', $
		pro_set_value = 'PMI__Button__Control__iBEAt_BatchProcessMultiple' )

end
