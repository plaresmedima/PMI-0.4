;Returns the population-averaged AIF from Parker et al MRM 2006 (units: mM)

;IDL> utime = 300*findgen(10000)/9999.
;IDL> plot, utime, ParkerAif(utime, t0=20)
;IDL> utime = 40+280*findgen(10000)/9999.
;IDL> oplot, utime, ParkerAif(utime, t0=20), psym=4

;IDL> utime = 1425.4*findgen(10000)/9999.
;IDL> plot, utime, ParkerAif(utime, t0=0)
;IDL> time = [0,8.34,16.7,25.0,34.4,42.7,51.1,59.4,67.8,76.1,84.5,92.8,101.2,109.5,117.8,126.2,134.5,142.9,151.2,159.6,539.4,547.7,556.1,564.4,1059.1,1067.4,1075.8,1084.1,1400.4,1408.7,1417.1,1425.4]
;IDL> oplot, time, ParkerAif(time, t0=0), psym=4

;IDL> baseline = 13
;IDL> t0 = time[baseline-1]
;IDL> oplot, utime, ParkerAif(utime, t0=t0)
;IDL> oplot, time, ParkerAif(time, t0=t0), psym=4

;IDL> baseline=3 & t0=time[baseline-1]
;IDL> plot, utime, ParkerAif(utime, t0=t0)
;IDL> print, int_tabulated(utime, ParkerAif(utime, t0=t0))
;IDL> oplot, time, ParkerAif(time, t0=t0), psym=4
;IDL> oplot, utime,  Interpol(ParkerAif(time, t0=t0), time, utime)
;IDL> print, int_tabulated(utime, Interpol(ParkerAif(time, t0=t0), time, utime))


function ParkerAif, time, baseline=n0, t0=t0 ;time in sec

    if n_elements(n0) ne 0 then t0=time[n0-1]
    if n_elements(t0) eq 0 then t0=0

    t = time - t0

	A1=0.809 & A2=0.330 & T1=0.17046 & T2=0.365 & s1=0.0563 & s2=0.132
	a=1.050 & b=0.1685 & s=38.078 & tt=0.483

	AIF = exp(-(t/60.-T1)^2/(2*s1^2))*A1/(s1*sqrt(2*!PI)) $
	    + exp(-(t/60.-T2)^2/(2*s2^2))*A2/(s2*sqrt(2*!PI)) $
	    + a*exp(-b*t/60.)/(1+exp(-s*(t/60.-tt)))

    neg = where(t lt 0, nneg)
    if nneg gt 0 then AIF[neg]=0

	return, AIF
end