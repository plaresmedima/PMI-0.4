


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		OVERRIDES                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		REQUIRED                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



PRO MoCoModel_VariableFlipAngle__model, X, P, S

  IF n_params() EQ 0 THEN RETURN

  TR = X[0]
  FA = X[1:*]

  E = exp(-P[1]*TR)
  S = P[0] * sin(FA) * (1-E)/(1-cos(FA)*E)
END



FUNCTION MoCoModel_VariableFlipAngle::PIXEL_PARAMETERS, S, FIT=F

  nP = self.nr_of_free_parameters
  parinfo = replicate({limited:bytarr(2), limits:dindgen(2)}, nP)
  parinfo[*].limited[0] = 1

  P = [2*max(S), 1/1000.0]
  F = mpcurvefit(*self.fixed_parameters, S, 1+0E*S, P, function_name='MoCoModel_VariableFlipAngle__model',/quiet,PARINFO=parinfo,/NODERIVATIVE)

  RETURN, P

END



FUNCTION MoCoModel_VariableFlipAngle::PIXEL_FORWARD, P, S

  MoCoModel_VariableFlipAngle__model, *self.fixed_parameters, P, F
  RETURN, F
END




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		CLEANUP, INIT and DEFINE  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



PRO MoCoModel_VariableFlipAngle::CLEANUP

  ptr_free, self.fixed_parameters
END


FUNCTION MoCoModel_VariableFlipAngle::INIT, fixed_parameters

  X = fixed_parameters
  X[1:*] = !PI*X[1:*]/180.

  self.nr_of_free_parameters = 2
  self.fixed_parameters = PTR_NEW(X)

  RETURN, 1
END


PRO MoCoModel_VariableFlipAngle__DEFINE

  struct = {$
    MoCoModel_VariableFlipAngle, $
    INHERITS MoCoModel, $
    fixed_parameters: ptr_new() $ ;[TR in msec, flip angles in radians]
  }

END