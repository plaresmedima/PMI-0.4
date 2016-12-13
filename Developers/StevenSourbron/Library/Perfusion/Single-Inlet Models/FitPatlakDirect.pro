function FitPatlakDirect, time, Conc, aif, Interval=Ind, Pars=Pars, PatlakX=X, PatlakY=Y, ChiSq=ChiSq

	X = IntVector(time,aif)/aif
	Y = Conc/aif

	sorted = sort(X)
	X = X[sorted]
	Y = Y[sorted]
;
	Xfit = X
	Yfit = Y

	;select only the points in the given Patlak time interval
	if n_elements(Ind) ne 0 then begin
		if Ind[1]-Ind[0] lt 1 then return, Y
		Xfit = X[Ind[0]:Ind[1]]
		Yfit = Y[Ind[0]:Ind[1]]
	endif

	;select only the points with finite values
	i = where(finite(Yfit) and finite(Xfit), cnt)
	if cnt lt 2 then return, Y
	Xfit = Xfit[i]
	Yfit = Yfit[i]

	Pars = LinFit(Xfit,Yfit,ChiSq=ChiSq)

	return, Pars[0] + Pars[1]*X
end