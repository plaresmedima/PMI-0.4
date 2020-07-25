;P = [Sfat, T2*fat, Swater, T2*water]

PRO MoCoModel_T2star__Model, TE, P, S

;Fat-water frequency shift in radians per ms
;Assuming first two echoes are in phase and opposed

	f = !PI/(TE[1]-TE[0])

;Initial echo assumed to be opposed-phase

    phase = !PI + f*(TE - TE[0])

	Sfat = P[0]*exp(-TE/P[1])
	Swat = P[2]*exp(-TE/P[3])

	Sx = Sfat*cos(phase) + Swat
	Sy = Sfat*sin(phase)

	S = SQRT(Sx^2 + Sy^2)

END


;Inverse model on single pixel level
;Input: signals S for a single pixel
;Returns: fitted parameters P

FUNCTION MoCoModel_T2star::PIXEL_PARAMETERS, S, FIT=F

  parinfo = replicate({limited:[1B,0B], limits:[0D,1D]}, 4)
  P = [0.01*max(S),10.0,0.99*max(S),40.0]
  F = mpcurvefit(*self.fixed, S, 1+0E*S, P, $
    function_name='MoCoModel_T2star__Model',/quiet, PARINFO=parinfo, /NODERIVATIVE)

  IF ARG_PRESENT(F) THEN F = self->PIXEL_FORWARD(P)

  RETURN, P

END


;Forward model on single-pixel level
;Input: Model parameters P = 1D array with parameters for 1 pixel
;Returns: fitted signals S for a single pixel

FUNCTION MoCoModel_T2star::PIXEL_FORWARD, P, S

  MoCoModel_T2star__Model, *self.fixed, P, S
  RETURN, S
END



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		CLEANUP, INIT and DEFINE  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



PRO MoCoModel_T2star::CLEANUP

  ptr_free, self.fixed
END



FUNCTION MoCoModel_T2star::INIT, f

  self.nr_of_free_parameters = 4
  self.fixed = ptr_new(f.TE)

  RETURN, 1
END



PRO MoCoModel_T2star__DEFINE

  struct = {$
    MoCoModel_T2star, $
    INHERITS MoCoModel, $
    fixed: ptr_new() $
  }

END