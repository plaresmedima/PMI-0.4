;Calculates the effect of an arbitrary sequence of MR-pulses on the longitudinal magnetisation

;Calling sequence for N identical pulses:

; Mz = Propagate_Mz(Mz, Meq, T1, FA, TR, N)

;Calling sequence for an arbitrary sequence of N pulses:

; Mz = Propagate_Mz(Mz, Meq, T1, [FA1,...,FAN], [TR1,TR2,...,TRN])

function Propagate_Mz, Minit, Meq, T1, FA, TR, N

	if n_params() eq 6 then begin
		E = exp(-TR/T1)
		c = cos(FA*!PI/180)
		Mss = Meq*(1-E)/(1-c*E)
		Mfinal = Mss + (Minit - Mss)*(c*E)^N
		return, Mfinal
	endif

	Mfinal = Minit
	for i=0L,n_elements(FA)-1 do begin
		E = exp(-TR[i]/T1)
		c = cos(FA[i]*!PI/180)
		Mfinal = Mfinal*c*E + Meq*(1-E)
	endfor
	return, Mfinal
end