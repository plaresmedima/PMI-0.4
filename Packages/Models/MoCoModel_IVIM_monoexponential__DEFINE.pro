;P = [S0, ADC]

PRO MoCoModel_IVIM_monoexponential__Model, b, P, S

    S = P[0]*exp(-b*P[1]) ; S =  S0exp(-b*ADC)

END


;Inverse model on single pixel level
;Input: signals S for a single pixel
;Returns: fitted parameters P

FUNCTION MoCoModel_IVIM_monoexponential::PIXEL_PARAMETERS, S, FIT=F

  parinfo = replicate({limited:[1B,0B], limits:[0D,1D]}, 2)
  P = [max(S),0.0025]
  F = mpcurvefit(*self.b, S, 1+0E*S, P, $
    function_name='MoCoModel_IVIM_monoexponential__Model',/quiet, PARINFO=parinfo, /NODERIVATIVE)

  IF ARG_PRESENT(F) THEN F = self->PIXEL_FORWARD(P)

  RETURN, P

END


;Forward model on single-pixel level
;Input: Model parameters P = 1D array with parameters for 1 pixel
;Returns: fitted signals S for a single pixel

FUNCTION MoCoModel_IVIM_monoexponential::PIXEL_FORWARD, P, S

  MoCoModel_IVIM_monoexponential__Model, *self.b, P, S
  RETURN, S
END



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		CLEANUP, INIT and DEFINE  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



PRO MoCoModel_IVIM_monoexponential::CLEANUP

  ptr_free, self.b
END



FUNCTION MoCoModel_IVIM_monoexponential::INIT, fixed_parameters

  self.nr_of_free_parameters = 2
  self.b = ptr_new(fixed_parameters.b)

  RETURN, 1
END



PRO MoCoModel_IVIM_monoexponential__DEFINE

  struct = {$
    MoCoModel_IVIM_monoexponential, $
    INHERITS MoCoModel, $
    b: ptr_new() $
  }

END