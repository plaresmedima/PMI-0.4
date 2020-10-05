;P = [S0, T2 Map]

PRO MoCoModel_T2Map__Model, PrepTime, P, S


    S = P[0]*exp(-PrepTime/P[1]) ; S =  S0exp(-TE/T2)


END


;Inverse model on single pixel level
;Input: signals S for a single pixel
;Returns: fitted parameters P

FUNCTION MoCoModel_T2Map::PIXEL_PARAMETERS, S, FIT=F

  parinfo = replicate({limited:[1B,0B], limits:[0D,1D]}, 2)
   P = [max(S), 70.0]
  F = mpcurvefit(*self.fixed, S, 1+0E*S, P, $
    function_name='MoCoModel_T2Map__Model',/quiet, PARINFO=parinfo, /NODERIVATIVE)

  IF ARG_PRESENT(F) THEN F = self->PIXEL_FORWARD(P)

  RETURN, P

END


;Forward model on single-pixel level
;Input: Model parameters P = 1D array with parameters for 1 pixel
;Returns: fitted signals S for a single pixel

FUNCTION MoCoModel_T2Map::PIXEL_FORWARD, P, S

  MoCoModel_T2Map__Model, *self.fixed, P, S
  RETURN, S
END



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		CLEANUP, INIT and DEFINE  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



PRO MoCoModel_T2Map::CLEANUP

  ptr_free, self.fixed
END



FUNCTION MoCoModel_T2Map::INIT, f

  self.nr_of_free_parameters = 2
  self.fixed = ptr_new(f.PrepTime)

  RETURN, 1
END



PRO MoCoModel_T2Map__DEFINE

  struct = {$
    MoCoModel_T2Map, $
    INHERITS MoCoModel, $
    fixed: ptr_new() $
  }

END