;IDL> time = [0,8.34,16.7,25.0,34.4,42.7,51.1,59.4,67.8,76.1,84.5,92.8,101.2,109.5,117.8,126.2,134.5,142.9,151.2,159.6,539.4,547.7,556.1,564.4,1059.1,1067.4,1075.8,1084.1,1400.4,1408.7,1417.1,1425.4]
;IDL> AIF = 1.050*exp(-0.1685*time/60.)/(1+exp(-38.078*(time/60.-0.483)))
;IDL> plot, time, AIF, psym=4
;IDL> AifNorm = NormaliseAifToParker(Time, AIF, 4)
;IDL> plot, time, ParkerAif(Time, Baseline=4), linestyle=1
;IDL> oplot, time, AifNorm


FUNCTION NormaliseAifToParker, Time, AIF, Baseline

    dt = 0.01 ;s
    nt = 1 + floor(max(time)/dt)
    utime = dt * findgen(nt)

    AifPop = ParkerAif(utime, t0=time[Baseline-1]) ;Plasma concentration in mM
    AifMeas = Interpol(Aif, Time, utime)

    tmax = time[Baseline-1] + 300.
    imax = 1 + floor(tmax/dt)

    AreaPop = int_tabulated(utime[0:imax], AifPop[0:imax])
    AreaMeas = int_tabulated(utime[0:imax], AifMeas[0:imax])

	return, Aif * (AreaPop/AreaMeas)

END