function RegridDeconvolutionData, time, aif, time_regr, aif_regr

	time_regr = time
	aif_regr = aif

	n = n_elements(time)
	dtime = time[1:n-1]-time[0:n-2]
	dt = min(dtime,max=mdt)
	if dt eq 0 then dt=1.0
	if (mdt-dt)/dt lt 0.1 then return, 0

    nt = floor(time[n-1]/dt)
	if nt gt 300 then begin
		nt = 300.
		dt = time[n-1]/nt
	endif
	time_regr = dt*dindgen(1+nt)
	aif_regr = interpol(aif,time,time_regr)

	return, 1
end