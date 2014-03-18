function PulseWaveVelocityTime_FindFoot, Time, Curve, Fit=Fit

	Max = max(Curve, TTP)
	Min = min(Curve[0:TTP],TTM)

	if Max eq Min then return, Time[0]

	C = Curve - Min
	Max = Max - Min

	Upslope = where((C[0:TTP] LT 0.8*Max) AND (C[0:TTP] GT 0.2*Max), cnt)
	if cnt eq 0 then Upslope = [TTM,TTP]
	if cnt eq 1 then Upslope = [Upslope-1,Upslope,Upslope+1]

	AB = LINFIT(Time[upslope], C[upslope])
	Foot = -AB[0]/AB[1]

	if arg_present(Fit) then Fit = Min + AB[0] + AB[1]*time

	return, Foot
end

function PulseWaveVelocityTime, Time, Des, Asc, Delay=Delay

	Foot_Asc = PulseWaveVelocityTime_FindFoot(Time, Asc, Fit=Asc_Fit)
	Foot_Des = PulseWaveVelocityTime_FindFoot(Time, Des, Fit=Des_Fit)

	Delay = Foot_Des - Foot_Asc
	return, Des

	;return, ShiftAif(Asc,Time,Delay)
	return, ShiftAif(Des,Time,-Delay)
end