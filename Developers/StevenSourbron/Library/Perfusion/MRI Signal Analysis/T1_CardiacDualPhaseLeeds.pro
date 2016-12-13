;Calculates the T1 from a signal S and a reference signal S0 with known T10
;measured with the cardiac dual-phase sequence (Leeds)

;ARGUMENTS:
;S = Signal S(C) at concentration C
;S0 = Precontrast signal S(C=0)
;T10 = Precontrast T1 in msec

;RETURN VALUE:
;Concentration in mM

Function T1_CardiacDualPhaseLeeds_Mz, T1

	TI = 150.0
	Ntot = 21
	Nswp = 12
	TR = 2.70
	FA = 15.0

	i = 1+lindgen(Nswp)

   	mz = Propagate_Mz(1,1,T1,90.,TI-Ntot*TR)
   	mz = Propagate_Mz(mz,1,T1,FA*i*(2*Nswp-i)/Nswp^2,TR*(1+i*0))
   	mz = Propagate_Mz(mz,1,T1,FA,TR,Ntot-Nswp)

	return, mz
end


Function T1_CardiacDualPhaseLeeds, S, S0, T10

	dT1 = 10.0 & T1i=1.0 & T1f=10000.	;Determine T1 with precision 10msec with Min=1msec and Max=10s.
	nT1 = 1 + floor((T1f-T1i)/dT1)
	T1 = T1i + dT1*findgen(nT1)

	mz0 = T1_CardiacDualPhaseLeeds_Mz(T10)
	mz = T1_CardiacDualPhaseLeeds_Mz(T1)

	return, interpol(T1, mz, mz0*S/S0)
end