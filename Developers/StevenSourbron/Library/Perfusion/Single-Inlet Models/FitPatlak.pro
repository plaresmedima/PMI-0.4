function FitPatlak, time, Conc, aif, Interval=t, Pars=Pars, PatlakX=X, PatlakY=Y, ChiSq=ChiSq

	Pars=[0,0]

	Xi = IntVector(time,aif)
	X = Xi/aif
	Y = Conc/aif

	;select only the points in the given time interval
	if n_elements(t) ne 0 then begin
		i = where((time ge t[0]) and (time le t[1]),cnt)
		if cnt lt 2 then return, Conc
		X=X[i] & Y=Y[i]
	endif

	;select only the points with finite values
	i = where(finite(Y) and finite(X), cnt)
	if cnt lt 2 then return, Conc
	X=X[i] & Y=Y[i]

	Pars = LinFit(X,Y,ChiSq=ChiSq)

	return, Pars[0]*aif + Pars[1]*Xi
end