
FUNCTION MOCOMO_2D_GROUPWISE::CHISQ, deformed, parameters

  RETURN, self.model->CHISQ(parameters, deformed)
END




PRO MOCOMO_2D_GROUPWISE::GRADIENT_ALL, Gradient, source, deformed, deformation_field, parameters, xc, yc, Weight_cnt, Weight_loc, Weight_val

  ns = SIZE(Source, /Dimensions)
  ng = size(Gradient,/Dimensions)

  res = - self.model -> GROUP_RESIDUE(parameters, deformed)

  FOR k=0L, ns[0]-1 DO BEGIN

    Sx = Source[k,1:ns[1]-1,*] - Source[k,0:ns[1]-2,*] ;derivative of source image with respect to x
    Sy = Source[k,*,1:ns[2]-1] - Source[k,*,0:ns[2]-2] ;derivative of source image with respect to y

    Gradient_k = Gradient[k,*,*]
    self->GRADIENT, Gradient_k, Sx, Sy, res[k,*,*], deformation_field[k,*,*,*], xc, yc, Weight_cnt, Weight_loc, Weight_val
    Gradient[k,*,*] = Gradient_k

  ENDFOR

END





FUNCTION MOCOMO_2D_GROUPWISE::FIT_ALL, deformed, deformation_field, Source, parameters, xc, yc, Weight_cnt, Weight_loc, Weight_val

  stepsize = 1.0 ;initial stepsize in pixels

  nf = size(deformation_field,/Dimensions)
  Gradient = FLTARR(nf[0], nf[1], Product(nf[2:*]), /NOZERO)

  FOR steps=0L, self.max_number_of_gradient_descent_steps DO BEGIN

    self -> GRADIENT_ALL, Gradient, Source, deformed, deformation_field, parameters, xc, yc, Weight_cnt, Weight_loc, Weight_val
    converged = self->LINESEARCH(Gradient, Source, deformed, deformation_field, parameters, xc, yc, STEPSIZE=stepsize)

    IF converged THEN BREAK

  ENDFOR

  RETURN, steps EQ 0
END




PRO MOCOMO_2D_GROUPWISE::OPTIMIZE, deformed, deformation_field, Source

  nd = SIZE(deformed, /DIMENSIONS)
  nf = SIZE(deformation_field, /DIMENSIONS)

  self->WEIGHTS, nf[2:*], nd[1:*], xc, yc, Weight_cnt, Weight_loc, Weight_val

  FOR updates=1L, self.max_number_of_parameter_updates DO BEGIN

print, 'update', updates

    self.model -> PARAMETERS, deformed, parameters
    no_change = self->FIT_ALL(deformed, deformation_field, source, parameters, xc, yc, Weight_cnt, Weight_loc, Weight_val)
    IF no_change THEN BREAK

  ENDFOR
END



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		CLEANUP, INIT and DEFINE  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




PRO MOCOMO_2D_GROUPWISE__DEFINE

  struct = {MOCOMO_2D_GROUPWISE $
    , INHERITS MOCOMO_2D $
    }


END