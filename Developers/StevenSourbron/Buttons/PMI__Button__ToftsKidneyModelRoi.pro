pro PMI__Button__Event__ToftsKidneyModelRoi, ev

	PMI__Info, ev.top, Stdy=Stdy

    Series = Stdy->Names(0,ns,DefDim=3,ind=ind,sel=sel)
    Regions = Stdy->Names(1,nr)
    Units = ['Signal Enhancement','Relative Signal Enhancement']

	v = PMI__Form(ev.top, Title='Perfusion analysis setup', [$
		ptr_new({Type:'DROPLIST',Tag:'series', Label:'Dynamic series', Value:Series, Select:sel}), $
		ptr_new({Type:'DROPLIST',Tag:'aif'	 , Label:'Arterial Region', Value:Regions, Select:stdy->sel(1)}), $
		ptr_new({Type:'DROPLIST',Tag:'roi'	 , Label:'Tissue Region', Value:Regions, Select:stdy->sel(1)}), $
		ptr_new({Type:'VALUE'	,Tag:'nbase' , Label:'Length of baseline (# of dynamics)', Value:20L}),$
		ptr_new({Type:'VALUE'	,Tag:'hct'	 , Label:'Patients hematocrit', Value:0.45})])
	IF v.cancel THEN return

	Display = PMI__DisplayNew(ev.top, 'PMI__DISPLAY__TOFTSKIDNEYMODELROI',$
		Series = Stdy->Obj(0,ind[v.series]), $
		Baseline = v.nbase, $
		Hematocrit = v.hct, $
		set_droplist_select = [v.aif,v.roi])

	PMI__Control, ev.top, MenuSensitive=0
end

pro PMI__Button__Control__ToftsKidneyModelRoi, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 1)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__ToftsKidneyModelRoi, parent,value=value, separator=separator

	PMI__Display__ToftsKidneyModelROI__Define

	if n_elements(value) eq 0 then value = 'Tofts Kidney Model (ROI)'

	return, widget_button(parent, $
		value = value,	$
		event_pro = 'PMI__Button__Event__ToftsKidneyModelRoi',	$
		pro_set_value = 'PMI__Button__Control__ToftsKidneyModelRoi', $
	 	separator = separator )
end
