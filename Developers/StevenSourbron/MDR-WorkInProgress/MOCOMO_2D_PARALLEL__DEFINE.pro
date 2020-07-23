
PRO MOCOMO_2D_PARALLEL::GRADIENT_ALL, Gradient, source, deformed, deformation_field, target, xc, yc, Weight_cnt, Weight_loc, Weight_val

  ns = SIZE(Source, /Dimensions)
  ng = size(Gradient,/Dimensions)

  FOR k=0L, ns[0]-1 DO BEGIN

    Sx = Source[k,1:ns[1]-1,*] - Source[k,0:ns[1]-2,*] ;derivative of source image with respect to x
    Sy = Source[k,*,1:ns[2]-1] - Source[k,*,0:ns[2]-2] ;derivative of source image with respect to y

    Gradient_k = Gradient[k,*,*]
    self->GRADIENT, Gradient_k, Sx, Sy, target[k,*,*]-deformed[k,*,*], deformation_field[k,*,*,*], xc, yc, Weight_cnt, Weight_loc, Weight_val
    Gradient[k,*,*] = Gradient_k

  ENDFOR

END



FUNCTION MOCOMO_2D_PARALLEL::FIT_ALL, deformed, deformation_field, Source, Target, xc, yc, Weight_cnt, Weight_loc, Weight_val

  stepsize = 20.0 ;initial stepsize in pixels

  nf = size(deformation_field,/Dimensions)
  Gradient = FLTARR(nf[0], nf[1], Product(nf[2:*]), /NOZERO)

  FOR steps=0L, self.max_number_of_gradient_descent_steps DO BEGIN

    self->GRADIENT_ALL, Gradient, Source, deformed, deformation_field, Target, xc, yc, Weight_cnt, Weight_loc, Weight_val
    converged = self->LINESEARCH(Gradient, Source, deformed, deformation_field, Target, xc, yc, STEPSIZE=stepsize)
    IF converged THEN BREAK

  ENDFOR

  RETURN, steps EQ 0
END



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		CLEANUP, INIT and DEFINE  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




PRO MOCOMO_2D_PARALLEL__DEFINE

  struct = {MOCOMO_2D_PARALLEL $
    , INHERITS MOCOMO_2D $
    }


END