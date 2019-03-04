FUNCTION FFD_3D_LSEARCH, Grad, Source, Target, xc, yc, zc, precision, DEFORMATION_FIELD = Field, DEFORMED = Def, STEPSIZE=stepsize

  scale_down = 1.5
  babystep = 0.1
  stepsize_max = 100.;emergency stop

  Grad /= sqrt(max(total(Grad^2,1)))
  ChiSq = total((Target - Def)^2.)

  ;backtrack in big steps until reduction in ChiSq

  WHILE 1 DO BEGIN

	F_try = Field + stepsize*Grad
	D_try = FFD_3D(Source, F_try, xc, yc, zc)
	ChiSq_try = total((Target - D_try)^2.)

	IF ChiSq_try LT ChiSq THEN BREAK
	IF stepsize LT precision THEN RETURN, 1B ;Converged

	stepsize /= scale_down

  ENDWHILE

  ;forward in baby steps until increase in chisq

  Grad *= babystep

  WHILE 1 DO BEGIN

    Field = F_try
    Def = D_try
    ChiSq = ChiSq_try

    stepsize += babystep
    F_try += Grad
    D_try = FFD_3D(Source, F_try, xc, yc, zc)
	ChiSq_try = total((Target-D_try)^2.)

    IF ChiSq_try GE ChiSq THEN RETURN, 0B
	IF stepsize GT stepsize_max THEN RETURN, 0B ;emergency stop

  ENDWHILE

END