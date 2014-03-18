function PMI__Button__Input__PulseWaveVelocityTime, ev, Time, Asc, Des

	PMI__Info, ev.top, Status=status, Stdy=Stdy

	Series = Stdy->Names(0,DefDim=3,ind=ind,sel=sel)
	Regions = Stdy->names(1)
	in = PMI__Form(ev.top, Title='Foot-to-Foot method', [$
		ptr_new({Type:'DROPLIST',Tag:'ser', Label:'Dynamic series', Value:Series, Select:sel}), $
		ptr_new({Type:'DROPLIST',Tag:'asc', Label:'Ascending ROI', Value:Regions, Select:stdy->sel(1)}), $
		ptr_new({Type:'DROPLIST',Tag:'des', Label:'Descending ROI', Value:Regions, Select:stdy->sel(1)}) ])
	IF in.cancel THEN return, 0
	Series = Stdy->Obj(0,ind[in.ser])

	;Possible formats:
	;CASE 1: Series consists of three images: magnitude, magnitude*abs(phase), phase (velocity)
	;CASE 2: Series consists of two images: magnitude, phase
	;CASE 3: Series consists of one image: phase
	nt = Series->Get('2001'x,'1017'x, error=error) ;number of phases
	if error eq 1 then begin ;CASE 3
		nt = n_elements(Series->t())
		nser = 1L
	endif else begin ;CASE 1&2
		nt = nt[0]
		nser = Series->d(3)/nt ;number of series (2 or 3)
	endelse

	;Express times in msec
	Time = Series->t()
	Time = Time-Time[0]
	HeartRate = Series->GetValue('0018'x,'1088'x, error=error) ;beats per min
	If error eq 1 then begin
		hr = PMI__Form(ev.top, Title='Please enter the heart rate', [$
			ptr_new({Type:'VALUE', Tag:'rate' , Label:'Heart rate (beats per min)', Value:70E}) ])
		IF hr.cancel THEN return, 0
		Series->Set, obj_new('DATA_ELEMENT','0018'x,'1088'x,value=hr.rate,name='Heart Rate',VR='IS')
		HeartRate = hr.rate
	endif
	HeartRate = HeartRate/60E ;beats per sec
	Time = Time/(HeartRate*nt)  ;time in units of sec

	;FOR CASES 1&2:
	;ROIs are drawn on the magnitude image, but should be evaluated on the velocity image
	;=> Shift Series time coordinates to extract ROI curves, then restore
	Series -> t, shift(Series->t(),(nser-1)*nt)
	Asc = PMI__RoiCurve(Stdy->DataPath(),Series,Stdy->Obj(1,in.asc),status,cnt=cntA,X=TA)
	Des = PMI__RoiCurve(Stdy->DataPath(),Series,Stdy->Obj(1,in.des),status,cnt=cntD,X=TD)
;	Asc = interpol(Asc,TA,Series->t()) ;some time points may have been removed by the user
;	Des = interpol(Des,TD,Series->t())
	Series -> t, shift(Series->t(),-(nser-1)*nt)

	if (cntA eq 0) or (cntD eq 0) then begin
		msg = dialog_message(/information,['One of the ROIs is not defined on this series'])
		return, 0
	endif

	Asc = Asc[(nser-1)*nt:nser*nt-1]
	Des = -Des[(nser-1)*nt:nser*nt-1]
	Time = Time[0:nt-1]

	return, 1
end



pro PMI__Button__Event__PulseWaveVelocityTime, ev

	if not PMI__Button__Input__PulseWaveVelocityTime(ev, Time, Asc, Des) $
	then begin
		PMI__Info, ev.top, Status=status
		PMI__Message, Status
		return
     endif

	Fit = PulseWaveVelocityTime(Time,Des,Asc,Delay=Pd)

	PMI__Control, ev.top, Viewer='PMI__Display__PerfusionRoiOutput', Display=Display

	Display -> Set, /Refresh $
	,	Model = 'Foot-to-Foot (Blue = Asc, Red = Desc)' $
	,   Style=1 $
	,	Time = Time $
	,	Curve = Asc $
	,	Fit = Fit $
	,	Units = 'Velocity (cm/sec)' $
	,	Parameters = [{Name:'Delay',Units:'msec', Value:1000D*Pd, Nr:1, Rnd:3} ]
end


pro PMI__Button__Control__PulseWaveVelocityTime, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 1)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__PulseWaveVelocityTime, parent,value=value, separator=separator

	PMI__Display__PerfusionRoiOutput__Define

	if n_elements(value) eq 0 then value = 'Pulse Wave Velocity - Foot-to-Foot Method'

	id = widget_button(parent $
	,	value 		= value	$
	,	event_pro 	= 'PMI__Button__Event__PulseWaveVelocityTime'$
	,	pro_set_value 	= 'PMI__Button__Control__PulseWaveVelocityTime' $
	, 	separator 	= separator	)

	return, id

end
