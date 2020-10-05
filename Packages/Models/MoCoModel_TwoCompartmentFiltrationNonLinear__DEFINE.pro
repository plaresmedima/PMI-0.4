


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		OVERRIDES                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



FUNCTION MoCoModel_TwoCompartmentFiltrationNonLinear::PIXEL_RESIDUE_PDER, P, S, k

  S_k = S*0
  S_k[k] = 1

  RETURN, S_k

END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		REQUIRED                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


FUNCTION MoCoModel_TwoCompartmentFiltrationNonLinear::PIXEL_PARAMETERS, S, FIT=F

  n = n_elements(S)
  S0 = total(S[0:self.n0-1])/self.n0
  C = S - S0

  parinfo = replicate({limited:[1B,0B], limits:[0D,1D]}, 4)
  P = [100/6000E,6E,10/6000E,60E]
  F = mpcurvefit(*self.X, C, 1+0E*C, P, function_name='CONC_2CFM',/quiet, PARINFO=parinfo, /NODERIVATIVE)
  F += S0

  RETURN, P

END


FUNCTION MoCoModel_TwoCompartmentFiltrationNonLinear::PIXEL_FORWARD, P, S

  n = n_elements(S)
  S0 = total(S[0:self.n0-1])/self.n0

  CONC_2CFM, *self.X, P, C

  RETURN, S0 + C

END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		CLEANUP, INIT and DEFINE  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



PRO MoCoModel_TwoCompartmentFiltrationNonLinear::CLEANUP

  ptr_free, self.X
END


FUNCTION MoCoModel_TwoCompartmentFiltrationNonLinear::INIT, fixed_parameters

  self.nr_of_free_parameters = 4
  self.n0 = fixed_parameters.n0
  self.X = ptr_new([fixed_parameters.t,fixed_parameters.ca])

  RETURN, 1
END


PRO MoCoModel_TwoCompartmentFiltrationNonLinear__DEFINE

  struct = {$
    MoCoModel_TwoCompartmentFiltrationNonLinear, $
    INHERITS MoCoModel, $
    n0: 0L, $
    X: ptr_new() $
  }

END