;P = [S0, T2 Map]

PRO MoCoModel_T2starMapKidney__Model, TE, P, S

    S = P[0]*exp(-TE/P[1]) ; S =  S0exp(-TE/T2*)

END


;Inverse model on single pixel level
;Input: signals S for a single pixel
;Returns: fitted parameters P

FUNCTION MoCoModel_T2starMapKidney::PIXEL_PARAMETERS, S, FIT=F

   parinfo = replicate({limited:[1B,0B], limits:[0D,1D]}, 2)
   P = [max(S), 50.0]
   F = mpcurvefit(*self.TE, S, 1+0E*S, P, $
    function_name='MoCoModel_T2starMapKidney__Model',/quiet, PARINFO=parinfo, /NODERIVATIVE)

  IF ARG_PRESENT(F) THEN F = self->PIXEL_FORWARD(P)

  RETURN, P

END


;Forward model on single-pixel level
;Input: Model parameters P = 1D array with parameters for 1 pixel
;Returns: fitted signals S for a single pixel

FUNCTION MoCoModel_T2starMapKidney::PIXEL_FORWARD, P, S

  MoCoModel_T2starMapKidney__Model, *self.TE, P, S
  RETURN, S
END



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		CLEANUP, INIT and DEFINE  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



PRO MoCoModel_T2starMapKidney::CLEANUP

  ptr_free, self.TE
END



FUNCTION MoCoModel_T2starMapKidney::INIT, f

  self.nr_of_free_parameters = 2
  self.TE = ptr_new(f.TE)

  RETURN, 1
END



PRO MoCoModel_T2starMapKidney__DEFINE

  struct = {$
    MoCoModel_T2starMapKidney, $
    INHERITS MoCoModel, $
    TE: ptr_new() $
  }

END