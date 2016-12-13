;Calculates T1 from a signal S and a reference signal S0 with known T10
;measured with the cardiac CEMARC sequence (Leeds)

;ARGUMENTS:
;S = Signal S(C) at concentration C
;S0 = Precontrast signal S(C=0)
;T10 = Precontrast T1 in msec
;slice = 0,1 or 2

;RETURN VALUE:
;Concentration in mM

Function T1_CEMARC_Mz, T1, slice

	TI = 150.0
	Ntot = 54
	Nctr = 21
	Nswp = 12
	TR = 2.7
	FA = 15.0

	i = 1+lindgen(Nswp)
   	FAswp = FA*i*(2*Nswp-i)/Nswp^2
   	TRswp = TR*(1+i*0)

	mz = Propagate_Mz(0,1,T1,0, slice*(TI+(Ntot-Nctr)*TR) + TI-Nctr*TR)		;free recovery from saturation
	mz = Propagate_Mz(mz,1,T1,FAswp,TRswp)		;flip angle sweep
	mz = Propagate_Mz(mz,1,T1,FA,TR,Nctr-Nswp)	;Remaining pulses to k-space center

	return, mz
end

Function T1_CEMARC, S, S0, T10, slice

	dT1 = 10.0 & T1i=0.01 & T1f=10000.	;Determine T1 with precision 10msec with Min=1msec and Max=10s.
	nT1 = 1 + floor((T1f-T1i)/dT1)
	T1 = T1i + dT1*findgen(nT1)

	mz0 = T1_CEMARC_Mz(T10,slice)
	mz = T1_CEMARC_Mz(T1,slice)

	Snorm = mz0*S/S0

;	i = where(Snorm gt mz[0], cnt)
;	if cnt gt 0 then Snorm[i]=mz[0]

	return, interpol(T1, mz, Snorm)
end