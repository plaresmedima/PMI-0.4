;Returns the population-averaged AIF from Parker et al MRM 2006 (units: mM)

function ParkerAif, t, baseline=n0 ;time in sec

	A1=0.809 & A2=0.330 & T1=0.17046 & T2=0.365 & s1=0.0563 & s2=0.132
	a=1.050 & b=0.1685 & s=38.078 & tt=0.483

	AIF = exp(-(t/60.-T1)^2/(2*s1^2))*A1/(s1*sqrt(2*!PI)) $
	    + exp(-(t/60.-T2)^2/(2*s2^2))*A2/(s2*sqrt(2*!PI)) $
	    + a*exp(-b*t/60.)/(1+exp(-s*(t/60.-tt)))


    if n_elements(n0) gt 0 then begin

      if n0 eq 0 then return, AIF
      AIF = shift(AIF,n0)
      AIF[0:n0-1] = 0
    endif

	return, AIF
end