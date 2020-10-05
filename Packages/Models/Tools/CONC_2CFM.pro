;Description
;-----------
;
;Calculates the concentration-time curve for a
;two-compartment filtration model (2CFM) following
;the formula in Eq.6 Flouri et al 2016 MRM
;
;
;Syntax
;------
;
;CONC_2CFM, X, P, C
;
;
;Arguments
;--------
;
;X: an array [t, ca] where t are the time points and ca the input function
;   t and ca must have the same length
;
;P: a four element array [FP, TP, PS, TE] of tissue parameters
;
;C: name variable which upon return contains the result of the calculation
;   has the same length as t and ca
;
;
;Keywords
;--------
;
;None
;
;
;Example
;-------
;
;Plot concentrations for patient 3
;
;IDL> tacq = 180.0 & dt = 0.1
;IDL> nt = 1+floor(tacq/dt)
;IDL> t = findgen(nt)*dt
;IDL> CONC_2CFM, [t,AIF(t)], PARS(3), concentrations
;IDL> plot, t, AIF(t)
;IDL> oplot, t, concentrations




PRO CONC_2CFM, X, PARS, CONC

	N = n_elements(X)/2
	time = X[0:N-1]
	ca = X[N:2*N-1]

	FP = PARS[0]
	TP = PARS[1]
	PS = PARS[2]
	TE = PARS[3]

    IF FP EQ 0 THEN BEGIN
        CONC = time*0
        RETURN
    ENDIF

    VP = FP*TP
    VE = PS*TE
    T = (VP+VE)/FP

    Tpos = TE
    Tneg = TP

    IF Tpos EQ Tneg THEN BEGIN
        CONC = FP*Tpos*EXPCONV(Tpos, time, ca)
    ENDIF ELSE BEGIN
        Epos = (T-Tneg)/(Tpos-Tneg)
        Eneg = 1-Epos
    	CONC = FP*Epos*Tpos*EXPCONV(Tpos, time, ca) $
             + FP*Eneg*Tneg*EXPCONV(Tneg, time, ca)
    ENDELSE

END