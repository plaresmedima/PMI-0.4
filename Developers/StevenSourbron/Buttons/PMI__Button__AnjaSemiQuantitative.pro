;DESCRIPTION
;Calculates the area under the signal-enhancement curve of a time series
;The first time point of the series is ignored (transition to steady state)

;Written by Steven Sourbron, Klinikum Grosshadern, LMU Munich, 18-08-2009
;Written for Anja Weidner (prostate perfusion), University Hospital Mannheim

pro PMI__Button__Event__AnjaSemiQuantitative, ev

;Get the loaded study
    PMI__Info, ev.top, Status=Status, Stdy=Stdy
    PMI__Message, status, 'Calculating AUC'
;Get all series with a time dimension
    Series = Stdy->Names(0,ns,DefDim=3,ind=ind,sel=sel)
;Get user input
	v = PMI__Form(ev.top, Title='Area Under the Curve', $
		[ ptr_new({Type:'DROPLIST',Tag:'series',Label: 'Dynamic series', Value:Series, Select:sel}) $
		, ptr_new({Type:'VALUE'	,Tag:'nb',Label: 'Length of baseline (# of dynamics)', Value:8L}) ] $
		) & if v.cancel then return
;Get the selected series object and its properties
    Series = Stdy->Obj(0,ind[v.series])
    Time = float(Series->c(1))
    d = Series->d()
;Create the output series object
	AUC = Stdy->New('SERIES',Default=Series,Name='Area under the Curve (a.u.*min)')	& AUC->t, Time[1]
	im = fltarr(d[0]*d[1])
	maxim=0
;Loop over the slices
	for i=0L,d[2]-1 do begin
	;display the progress of the calculation to the user
		if d[2] gt 1 then PMI__Message, status, 'Calculating AUC', i/(d[2]-1E)
	;calculate baseline signal S0
		S0 = Series -> Read(Stdy->DataPath(),i,1)
		for j=2L,v.nB-1 do S0 = S0 + Series -> Read(Stdy->DataPath(),i,j)
		S0 = S0/(v.nB-1)
	;Loop over all times
		im = im*0
		for j=2L,d[3]-1 do begin
		;Load slice for time j
			SE = Series -> Read(Stdy->DataPath(),i,j) - S0
		;Add the area between time j-1 and j
			im = im + (Time[j]-Time[j-1])*SE/60.
		endfor
	;Store result for slice i
		AUC -> Write, Stdy->DataPath(), im, i
	;Update the maximum
		maxim = max([maxim,max(im)])
	endfor
;set the color window to the maximum pixel value
	AUC -> Trim, [0,maxim]
;refresh the display
    PMI__Control, ev.top, /refresh
end


pro PMI__Button__Control__AnjaSemiQuantitative, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		sensitive = ns gt 0
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__AnjaSemiQuantitative, parent,value=value,separator=separator

    if n_elements(value) eq 0 then value = 'Area under the curve'

    id = widget_button(parent  $
    ,   value      = value   $
    ,  event_pro   = 'PMI__Button__Event__AnjaSemiQuantitative' $
    ,	pro_set_value 	= 'PMI__Button__Control__AnjaSemiQuantitative' $
    ,  separator   = separator  )

    return, id
end

