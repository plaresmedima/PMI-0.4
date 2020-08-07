

pro PMI__Button__Event__SeriesEditDicomHeader, ev

    Pmi__info, ev.top, Stdy=Stdy, Series=Series

	stdy->saved, 0B eq cw_editdicomheader(Series, parent=tlb(ev.top))

end


pro PMI__Button__Control__SeriesEditDicomHeader, id, v

	PMI__Info, tlb(id), Series=Series
	widget_control, id, sensitive = obj_valid(Series)
end
function PMI__Button__SeriesEditDicomHeader, $
	parent, $
	separator=separator, $
	value=value

	if n_elements(value) eq 0 then value='Edit header'

    id = widget_button(parent $
    ,  	value    = value  $
    ,  	event_pro   = 'PMI__Button__Event__SeriesEditDicomHeader' $
    , 	pro_set_value 	= 'PMI__Button__Control__SeriesEditDicomHeader' $
    ,  	separator    = separator          )

    return, id

end