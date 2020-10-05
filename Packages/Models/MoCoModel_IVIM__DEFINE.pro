;P = [S0, Dxx, Dyy, Dzz, S0*, Dxx*, Dyy*, Dzz*]

PRO MoCoModel_IVIM__Model, b, P, S

	n = n_elements(b)/3
	bx = b[0:n-1]
	by = b[n:2*n-1]
	bz = b[2*n:*]

	S = FLTARR(3*n)
	S[0:n-1] 	= P[0]*exp(-P[1]*bx) + P[4]*exp(-P[5]*bx)
	S[n:2*n-1] 	= P[0]*exp(-P[2]*by) + P[4]*exp(-P[6]*by)
	S[2*n:*] 	= P[0]*exp(-P[3]*bz) + P[4]*exp(-P[7]*bz)

END


;Inverse model on single pixel level
;Input: signals S for a single pixel
;Returns: fitted parameters P

FUNCTION MoCoModel_IVIM::PIXEL_PARAMETERS, S, FIT=F

  parinfo = replicate({limited:[1B,0B], limits:[0D,1D]}, 8)
  P = [0.9*max(S),0.001,0.001,0.001,0.1*max(S),0.01,0.01,0.01]
  F = mpcurvefit(*self.b, S, 1+0E*S, P, $
    function_name='MoCoModel_IVIM__Model',/quiet, PARINFO=parinfo, /NODERIVATIVE)

  IF ARG_PRESENT(F) THEN F = self->PIXEL_FORWARD(P)

  RETURN, P

END


;Forward model on single-pixel level
;Input: Model parameters P = 1D array with parameters for 1 pixel
;Returns: fitted signals S for a single pixel

FUNCTION MoCoModel_IVIM::PIXEL_FORWARD, P, S

  MoCoModel_IVIM__Model, *self.b, P, S
  RETURN, S
END



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		CLEANUP, INIT and DEFINE  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



PRO MoCoModel_IVIM::CLEANUP

  ptr_free, self.b
END



FUNCTION MoCoModel_IVIM::INIT, fixed_parameters

  self.nr_of_free_parameters = 8
  self.b = ptr_new(fixed_parameters.b)

  RETURN, 1
END



PRO MoCoModel_IVIM__DEFINE

  struct = {$
    MoCoModel_IVIM, $
    INHERITS MoCoModel, $
    b: ptr_new() $
  }

END