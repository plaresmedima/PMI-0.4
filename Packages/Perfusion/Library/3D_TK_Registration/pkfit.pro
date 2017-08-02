FUNCTION pkfit, Time, Ca, St, n0, par=par

  n = size(St, /dimensions)
  S0 = total(St[*,*,*,0:n0-1],4)/n0
  S0 = rebin(S0,n[0],n[1],n[2],n[3])
  Ct = St - S0

  Fit = S0
  Par = fltarr(n[0],n[1],n[2],4)

  FOR i=0L, n[0]-1 DO BEGIN
  	FOR j=0L, n[1]-1 DO BEGIN
      FOR k=0L, n[2]-1 DO BEGIN
		Par[i,j,k,*] = LLS_2CFM(Time, REFORM(Ct[i,j,k,*]),ca, FIT=Cfit_ijk, WEIGHTS=w, /positivity)
		Fit[i,j,k,*] += Cfit_ijk
   	  ENDFOR
  	ENDFOR
  ENDFOR

  RETURN, Fit

END