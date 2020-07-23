FUNCTION MOCOMO_2D_INTERACTING_LINEAR::LINESEARCH, t, deformed, deformation_field, G_t, S_t, parameters, xc, yc, STEPSIZE=stepsize

  scale_down = 1.5
  babystep = 0.1*self.tolerance
  stepsize_max = 100.;emergency stop

  G_t /= sqrt(max(total(G_t^2,2)))
  ChiSq = self->ChiSq(deformed, parameters)

  ;backtrack in big steps until reduction in ChiSq

  stepsize_try = stepsize
  deformed_try = deformed

  WHILE 1 DO BEGIN

	F_try = deformation_field[t,*,*,*] + stepsize_try*G_t
	self->FFD, D_try, S_t, F_try, xc, yc
	deformed_try[t,*,*] = D_try
	ChiSq_try = self->ChiSq(deformed_try, parameters)

	IF ChiSq_try LT ChiSq THEN BREAK
	stepsize_try /= scale_down
    IF stepsize_try LT self.tolerance THEN RETURN, 1B ;Converged

  ENDWHILE

  ;forward in baby steps until increase in chisq

  G_t *= babystep

  WHILE 1 DO BEGIN

    stepsize = stepsize_try
    deformation_field[t,*,*,*] = F_try
    deformed[t,*,*] = D_try
    ChiSq = ChiSq_try

    stepsize_try += babystep
    F_try += G_t
    self->FFD, D_try, S_t, F_try, xc, yc
    deformed_try[t,*,*] = D_try
	ChiSq_try = self->ChiSq(deformed_try, parameters)

    IF ChiSq_try GE ChiSq THEN RETURN, 0B
	IF stepsize_try GT stepsize_max THEN RETURN, 0B ;emergency stop

  ENDWHILE

END




FUNCTION MOCOMO_2D_INTERACTING_LINEAR::FIT, t, deformed, deformation_field, Source, parameters, xc, yc, Weight_cnt, Weight_loc, Weight_val

  stepsize = 5.0 ;initial stepsize in pixels

  ns = SIZE(source, /Dimensions)
  nd = SIZE(deformed, /Dimensions)
  nf = SIZE(deformation_field,/Dimensions)

  S_t = Source[t,*,*]
  Sx_t = S_t[*,1:ns[1]-1,*] - S_t[*,0:ns[1]-2,*] ;derivative of source image with respect to x
  Sy_t = S_t[*,*,1:ns[2]-1] - S_t[*,*,0:ns[2]-2] ;derivative of source image with respect to y
  Rp_t = self.model -> RESIDUE_PDER(parameters, deformed, t) ;Linear so does not depend on deformed

  G_t = FLTARR(1, nf[1], Product(nf[2:*]), /NOZERO)

  FOR steps=0L, self.max_number_of_gradient_descent_steps DO BEGIN

print, steps

    self.model -> RESIDUE, deformed, parameters, residue
    residue_t = - TOTAL(residue * Rp_t, 1)
    residue_t = REFORM(residue_t, [1,nd[1:*]], /OVERWRITE)

    self->GRADIENT, G_t, Sx_t, Sy_t, residue_t, deformation_field[t,*,*,*], xc, yc, Weight_cnt, Weight_loc, Weight_val
    converged = self->LINESEARCH(t, deformed, deformation_field, G_t, S_t, parameters, xc, yc, STEPSIZE=stepsize)

    IF converged THEN BREAK

  ENDFOR

  RETURN, steps EQ 0
END



FUNCTION MOCOMO_2D_INTERACTING_LINEAR::FIT_ALL, deformed, deformation_field, source, parameters, xc, yc, Weight_cnt, Weight_loc, Weight_val

  ns = SIZE(source, /Dimensions)

  converged = 1B
  FOR t=ns[0]-1,0L,-1 DO BEGIN

print, 'time', t

     converged *= self->FIT(t, deformed, deformation_field, Source, Parameters, xc, yc, Weight_cnt, Weight_loc, Weight_val)
  ENDFOR

  RETURN, converged
END



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		CLEANUP, INIT and DEFINE  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





PRO MOCOMO_2D_INTERACTING_LINEAR__DEFINE

  struct = {MOCOMO_2D_INTERACTING_LINEAR $
    , INHERITS MOCOMO_2D_GROUPWISE $
    }


END