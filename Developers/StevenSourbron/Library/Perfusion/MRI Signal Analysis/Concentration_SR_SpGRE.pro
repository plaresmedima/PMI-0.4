;Calculates the tracer concentration C from a signal S(C) and a precontrast signal S(C=0)
;measured with a saturation-recovery T1-weighted spoiled gradient-echo sequence

;ARGUMENTS:
;S = Signal S(C) at concentration C
;S0 = Precontrast signal S(C=0)
;T10 = Precontrast T1 in msec
;r1 = Relaxivity in Hz/mM
;TD = delay time after saturation pulse (msec)
;FA = Flip angle in degrees
;TR = Repetition time TR in msec (=time between two pulses)
;N = Number of phase lines before k-space centre

;RETURN VALUE:
;Concentration in mM

function Concentration_SR_SpGRE_Mz, T1, TD, FA, TR, N

	mz = Propagate_Mz(1,1,T1,90.,TD)	;saturation pulse
	mz = Propagate_Mz(mz,1,T1,FA,TR,N)  ;alpha pulses

	return, mz
end

Function Concentration_SR_SpGRE, S, S0, T10, r1, TD, FA, TR, N

	dT1 = 10.0 & T1i=1.0 & T1f=10000.	;Determine T1 with precision 10msec with Min=1msec and Max=10s.
	nT1 = 1 + floor(T1f/T1i)
	T1 = T1i + dT1*findgen(nT1)

   	mz0 = Concentration_SR_SpGRE_Mz(T10,TD, FA, TR, N)
   	mz = Concentration_SR_SpGRE_Mz(T1,TD, FA, TR, N)

	T1 = interpol(T1, mz, mz0*S/S0)

	return, 1000*(1/T1 - 1/T10)/r1
end