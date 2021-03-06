;Calculates the tracer concentration C from a signal S(C) and a precontrast signal S(C=0)
;measured with a saturation-recovery sequence

;ARGUMENTS:
;S = Signal S(C) at concentration C
;S0 = Precontrast signal S(C=0)
;Saturation delay TD in msec (=time between saturation pulse and centre of k-space)
;Precontrast T10 in msec
;Relaxivity in Hz/mM

;RETURN VALUE:
;Concentration in mM

Function Concentration_SR, S, S0, T10, TI, r

	;WORK IN PROGRESS

	E = exp(-TR/T10)
	c = cos(FA*!PI/180)
	Sn = (S/S0)*(1-E)/(1-c*E)	;normalized signal
	R1 = -alog((1-Sn)/(1-c*Sn))/TR	;relaxation rate in 1/msec
	return, 1000*(R1 - 1/T10)/r
end