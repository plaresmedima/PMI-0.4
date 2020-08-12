



pro RegridDeconvolutionPixelData,time,p,aif,time_regr=time_regr,aif_regr=aif_regr

	if not RegridDeconvolutionData(time, aif, time_regr, aif_regr) then return

	d = size(p,/dimensions)
	nt = n_elements(time_regr)
	p_regr = fltarr(d[0],nt)
	for i=0L,d[0]-1 do begin
		curve = reform(p[i,*],/overwrite)
		p_regr[i,*] = interpol(curve,time,time_regr)
	endfor
	p = p_regr

end