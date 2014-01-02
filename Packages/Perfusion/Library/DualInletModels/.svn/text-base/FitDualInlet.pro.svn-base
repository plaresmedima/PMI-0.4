;DESCRIPTION: Fits a model with a dual inlet into the central (blood) compartment
;WRITTEN BY: Steven Sourbron, Leeds University
;INTRODUCED: 15 oct 2010

;CALLING SEQUENCE
;Fit = FitDualInlet(Model, Time, InputA, InputV, Residue, Pars)

;RETURN VALUES
;Fit: 	the best fit to the data

;REQUIRED ARGUMENTS
;Model		A string with the name of the model to be fitted (eg. 'FitSingleInletPatlak')
;Time: 		time points at which the arterial input function is defined (in seconds)
;InputA: 		the arterial input function (in arbitrary units)
;				must have the same number of elements as "time"
;InputV: 		the venous input function (in arbitrary units)
;				must have the same number of elements as "time"
;Residue: 	the tissue curve (in the same units as "input")
;				must have the same number of elements as "time"
;Pars: 		a named variable with an array of initial values for the model parameters
;				Upon return, "Pars" contains the values of the fitted parameters

;KEYWORD ARGUMENTS

;DELAY_VALUES=s		a three-element array defining the delay values over which should be fitted
;					if undefined, the model is fitted without a delay parameter
;						s[0] and s[1] are the smallest and largest delays under consideration (in seconds)
;						s[2] is the stepsize (in seconds)
;						eg. s = [0,20,0.5] fits with delay from 0 to 20 seconds in steps of 0.5 seconds
;					s[0] MUST be positive
;DELAY_WHICH=which	a byte value specifying whether both inputs are delayed (which=2), only the AIF (which=0), or only the VIF (which=1).
;						If absent, which=2 is assumed
;DELAY_PAR			A named variable that contains the value of the optimal delay time upon return
;INDICES=ti			indices of "time" which are used for the fit. If not defined, all time points are fitted.
;AKAIKE_ERROR=aic 	a named variable that contains the Akaike Fit error upon return
;LIMITED_BELOW		a binary array with the same number of elements as "Par"
;					set to 1 to fit the corresponding element in "Par" with a lower limit (default=not limited)
;LIMITS_BELOW		an array of lower limits, one element for each entry "1" in LIMITED_BELOW (default=0)
;LIMITED_ABOVE		a binary array with the same number of elements as "Par"
;					set to 1 to fit the corresponding element in "Par" with an upper limit (default=not limited)
;LIMITS_ABOVE		an array of upper limits, one element for each entry "1" in LIMITED_ABOVE (default=1)
;POSITIVITY 		If set, the parameter values are constrained to be positive
;NODERIVATIVE		If set, the derivatives with respect to the model parameters are calculated from the data



FUNCTION FitDualInletModel, Model, Time, InputA, InputV, Residue, Pars, $
	INDICES=ti, $
	AKAIKE_ERROR=aic, $
	FIXED=fixed, $
	LIMITED_BELOW=limited_below,$
	LIMITS_BELOW=limits_below, $
	LIMITED_ABOVE=limited_above, $
	LIMITS_ABOVE=limits_above, $
	POSITIVITY=positivity, $
	NODERIVATIVE=noderivative


	;SET LIMITS
	nP = n_elements(Pars)
	limited = bytarr(2)	;Default = not limited
	limits = dindgen(2)	;Default limits = [0,1]
	parinfo = replicate({limited:limited, limits:limits, fixed:0B}, nP)
	if keyword_set(positivity) then parinfo[*].limited[0] = 1+bytarr(nP)
	if n_elements(fixed) ne 0 then parinfo[fixed].fixed = 1B
  	if n_elements(limited_below) ne 0 then begin
  		parinfo[*].limited[0] = limited_below
  		if n_elements(limits_below) ne 0 then begin
  			i = where(limited_below, cnt)
  			if cnt gt 0 then parinfo[i].limits[0] = limits_below
  		endif
  	endif
  	if n_elements(limited_above) ne 0 then begin
  		parinfo[*].limited[1] = limited_above
  		if n_elements(limits_above) ne 0 then begin
  			i = where(limited_above, cnt)
  			if cnt gt 0 then parinfo[i].limits[1] = limits_above
  		endif
  	endif


	;CALCULATE FIT
	n = n_elements(Time)
	if n_elements(ti) eq 0 then ti=lindgen(n)
	ni = n_elements(ti)
	Fit = MpCurveFit([ni,ti,Time,InputA,InputV], Residue[ti], 1+0E*Residue[ti], Pars, function_name='DualInlet'+Model,/quiet,PARINFO=parinfo,NODERIVATIVE=noderivative)
	if ni ne n then CALL_PROCEDURE, 'DualInlet'+Model, [n,lindgen(n),Time,InputA,InputV], Pars, Fit
	if arg_present(aic) then aic = ni*alog(total((Residue[ti]-Fit[ti])^2)/ni) + 2D*(1+nP-n_elements(fixed))

	return, Fit
END


function FitDualInlet, Model, Time, InputA, InputV, Residue, Pars, DELAY_VALUES=s, DELAY_WHICH=w, DELAY_PAR=delay, AKAIKE_ERROR=aic, _EXTRA=e

	IF n_elements(w) EQ 0 THEN w=-1 ;Default = no delay

	IF w EQ -1 THEN $	;Fit without delay
		return, FitDualInletModel(Model,Time, InputA, InputV, Residue, Pars, AKAIKE_ERROR=aic, _EXTRA=e)

	n = 1 + floor((s[1]-s[0])/s[2])
	Delay = s[0] + s[2]*findgen(n)

	IF w EQ 2 THEN BEGIN
		Error = findgen(n,n)
		FOR i=0L,n-1 DO BEGIN
		FOR j=0L,n-1 DO BEGIN
			Init = Pars
			If Delay[i] gt 0 then InputA_del = ShiftAif(InputA,Time,Delay[i]) else InputA_del = InputA
			If Delay[j] gt 0 then InputV_del = ShiftAif(InputV,Time,Delay[j]) else InputV_del = InputV
			Fit = FitDualInletModel(Model, Time, InputA_del, InputV_del, Residue, Init, AKAIKE_ERROR=aic, _EXTRA=e)
			Error[i,j] = aic
		ENDFOR
		ENDFOR
		tmp = min(Error,i)
		r = reform_ind([n,n],ind=i)
		Delay = [Delay[r[0]],Delay[r[1]]]
		If Delay[0] gt 0 THEN InputA_del = ShiftAif(InputA,Time,Delay[0]) else InputA_del = InputA
		If Delay[1] gt 0 THEN InputV_del = ShiftAif(InputV,Time,Delay[1]) else InputV_del = InputV
		Fit = FitDualInletModel(Model,Time, InputA_del, InputV_del, Residue, Pars, AKAIKE_ERROR=aic, _EXTRA=e)
		aic = aic + 4D
	ENDIF ELSE BEGIN
		Error = findgen(n)
		FOR i=0L,n-1 DO BEGIN
			Init = Pars
			IF (w eq 0) AND (Delay[i] GT 0) THEN InputA_del = ShiftAif(InputA,Time,Delay[i]) else InputA_del = InputA
			IF (w eq 1) AND (Delay[i] GT 0) THEN InputV_del = ShiftAif(InputV,Time,Delay[i]) else InputV_del = InputV
			Fit = FitDualInletModel(Model, Time, InputA_del, InputV_del, Residue, Init, AKAIKE_ERROR=aic, _EXTRA=e)
			Error[i] = aic
		ENDFOR
		tmp = min(Error,i)
		Delay = Delay[i]
		IF (w eq 0) AND (Delay GT 0) THEN InputA_del = ShiftAif(InputA,Time,Delay) else InputA_del = InputA
		IF (w eq 1) AND (Delay GT 0) THEN InputV_del = ShiftAif(InputV,Time,Delay) else InputV_del = InputV
		Fit = FitDualInletModel(Model,Time, InputA_del, InputV_del, Residue, Pars, AKAIKE_ERROR=aic, _EXTRA=e)
		aic = aic + 2D
	ENDELSE

	return, Fit
end