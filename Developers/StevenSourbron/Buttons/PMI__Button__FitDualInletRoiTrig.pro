pro PMI__Button__Event__FitDualInletRoiTrig, ev

	PMI__Info, ev.top, Stdy=Stdy, Status=StatusId

    Series = Stdy->Names(0,ns,DefDim=3,ind=ind,sel=sel)
    Regions = Stdy->Names(1,nr)
    Units = ['Signal Enhancement','Relative Signal Enhancement']

	v = PMI__Form(ev.top, Title='Perfusion analysis setup', [$
		ptr_new({Type:'DROPLIST',Tag:'series', Label:'Dynamic series', Value:Series, Select:sel}), $
		ptr_new({Type:'DROPLIST',Tag:'roi'	 , Label:'Tissue Region', Value:Regions, Select:stdy->sel(1)}), $
		ptr_new({Type:'DROPLIST',Tag:'aif'	 , Label:'Arterial Region', Value:Regions, Select:stdy->sel(1)}), $
		ptr_new({Type:'DROPLIST',Tag:'vif'	 , Label:'Venous Region', Value:Regions, Select:stdy->sel(1)}), $
		ptr_new({Type:'DROPLIST',Tag:'tri'	 , Label:'Triggering Region', Value:Regions, Select:stdy->sel(1)}), $
		ptr_new({Type:'DROPLIST',Tag:'units' , Label:'Approximate tracer concentrations by:', Value:Units, Select:1}), $
		ptr_new({Type:'VALUE'	,Tag:'nbase' , Label:'Length of baseline (# of dynamics)', Value:10L}),$
		ptr_new({Type:'VALUE'	,Tag:'hct'	 , Label:'Patients hematocrit', Value:0.45})])
	IF v.cancel THEN return

	Tri = PMI__RoiCurve(Stdy->DataPath(),Stdy->Obj(0,ind[v.series]),Stdy->Obj(1,v.tri),StatusId,X=Time)
	TrigInd = RetrospectiveTriggeringIndices(Tri, Time[1]-Time[0], 1.0/20.0, 100, minima=0,cnt=cnt)

	PMI__Control, ev.top, Viewer = 'PMI__Display__FitDualInletRoiTrig', Display=Display

	Display -> Set, /Refresh, $
		Series = Stdy->Obj(0,ind[v.series]), $
		Units = Units[v.units], $
		Baseline = v.nBase, $
		Hematocrit = v.hct, $
		set_droplist_select = [v.roi,v.aif,v.vif], $
		TrigInd = TrigInd
end

pro PMI__Button__Control__FitDualInletRoiTrig, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 3)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__FitDualInletRoiTrig, parent,value=value, separator=separator

	PMI__Display__FitDualInletRoiTrig__Define

	if n_elements(value) eq 0 then value = 'Fit dual-inlet models (ROI - triggered)'

	id = widget_button(parent $
	,	value = value	$
	,	event_pro = 'PMI__Button__Event__FitDualInletRoiTrig'	$
	,	pro_set_value = 'PMI__Button__Control__FitDualInletRoiTrig' $
	, 	separator = separator)

	return, id
end
