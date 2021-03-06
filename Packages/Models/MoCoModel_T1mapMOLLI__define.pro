
;P: [A, B, T1apparent]

PRO MoCoModel_T1mapMOLLI__Model, TI, P, S

   S = P[0]-P[1]*exp(-TI/P[2])  ; S = A-B*exp(-TI/T1apparent)


END


;Inverse model on single pixel level
;Input: signals S for a single pixel
;Returns: fitted parameters P

FUNCTION MoCoModel_T1MapMOLLI::PIXEL_PARAMETERS, S, FIT=F


       null_point_index = min(S, indx) ; find min index

	   S[0:indx] = -  S[0:indx]

       P = [max(S),(max(S)-min(S)),1500.0] ; [687.0, 1329.0, 1500.0]

       F = mpcurvefit(* self.TI, S,  1+0E*S, P, function_name='MoCoModel_T1mapMOLLI__Model',/quiet, /NODERIVATIVE)

       F = ABS(F)

  RETURN, P

END


;Forward model on single-pixel level
;Input: Model parameters P = 1D array with parameters for 1 pixel
;Returns: fitted signals S for a single pixel

FUNCTION MoCoModel_T1mapMOLLI::PIXEL_FORWARD, P, S

  MoCoModel_T1mapMOLLI__Model, * self.TI, P, S
  RETURN, ABS(S)
END



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		CLEANUP, INIT and DEFINE  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



PRO MoCoModel_T1mapMOLLI::CLEANUP

  ptr_free,  self.TI
END



FUNCTION MoCoModel_T1mapMOLLI::INIT, f

  self.nr_of_free_parameters = 3 ; number of free params
  self.TI = ptr_new(f.TI) ;  ; fixed param

  RETURN, 1
END



PRO MoCoModel_T1mapMOLLI__DEFINE

  struct = {$
    MoCoModel_T1mapMOLLI, $
    INHERITS MoCoModel, $
    TI: ptr_new() $
  }

END





