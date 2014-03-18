;MODEL PARAMETERS

;P = [V, F, v, f]

;DEFINED AS FOLLOWS

;V = VP+VE	(Total Extracellular Volume)
;F (Plasma Flow)
;v = VE/(VP+VE)  (Extravascular Volume Fraction)
;f = 1-exp(-FE/F)  (Exchange Fraction)

;FITTED FUNCTION

;C(t) = FP R(t) * Ca(t) with:
;	t<VP/F: R(t)=1
;	t>VP/F: R(t)=E*exp(-(t-VP/F)*fF/VE)


Pro SingleInletAATH, X, P, C, C_DER

	if n_params() eq 0 then return

	ni=X[0] & n=n_elements(X[ni+1:*])/2
	ti=X[1:ni] & time=X[ni+1:ni+n] & input=X[ni+n+1:*]

	TP = (1-P[2])*P[0]/P[1]
	VE = P[2]*P[0]

	residue = time*0 + 1
	i = where(time gt TP, cnt)
	if cnt gt 0 then residue[i]=P[3]*exp(-(time[i]-TP)*P[3]*P[1]/VE)

	Conv = P[1]*ConvolveIrreg(time, input, residue)
	C = Conv[ti]

	IF n_params() LT 4 THEN return


end