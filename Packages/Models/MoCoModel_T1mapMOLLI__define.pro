;C(t) = Sinf*(1-Sratio*exp(-Td*R1)) @ Sirisha Tadimalla absolute function


PRO MoCoModel_T1mapMOLLI__Model, TI, P, S


	if n_params() eq 0 then return

	E = exp(-TI*P[2])

	C_DER0 = 1-P[1]*E

	S = ABS(P[0]*C_DER0)


END


;Inverse model on single pixel level
;Input: signals S for a single pixel
;Returns: fitted parameters P

FUNCTION MoCoModel_T1MapMOLLI::PIXEL_PARAMETERS, S, FIT=F

       ExpectedT1 = max(* self.TI)/4.0

       P = [max(S), 2.0, 1/ExpectedT1] ;[Sinf, Sratio(B/A), R1] @ Sirisha Tadimalla

	   F = mpcurvefit(* self.TI, S, 1+0E*S, P, function_name='MoCoModel_T1mapMOLLI__Model',/quiet,NODERIVATIVE=1)



  IF ARG_PRESENT(F) THEN F = self->PIXEL_FORWARD(P)

  RETURN, P

END


;Forward model on single-pixel level
;Input: Model parameters P = 1D array with parameters for 1 pixel
;Returns: fitted signals S for a single pixel

FUNCTION MoCoModel_T1mapMOLLI::PIXEL_FORWARD, P, S

  MoCoModel_T1mapMOLLI__Model, * self.TI, P, S
  RETURN, S
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





