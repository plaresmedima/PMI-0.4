;C(t) = Sinf*(1-Sratio*exp(-Td*R1)) @ Sirisha Tadimalla absolute function

;P = [A, B, T1apparent] ; original

PRO MoCoModel_T1mapMOLLI__Model, TI, P, S ;KS PRO is for definition in IDL; TI: inversion times (x); P: params to fit; S: dependent var (y): Signal


  ; S = P[0]-P[1]*exp(-TI/P[2])  ; S = A-B*exp(-TI/T1apparent) ; original function does not converge


	if n_params() eq 0 then return

	E = exp(-TI*P[2])

	C_DER0 = 1-P[1]*E

	S = ABS(P[0]*C_DER0)

	IF n_params() LT 4 THEN return

	C_DER1 = -P[0]*E
	C_DER2 = P[0]*P[1]*TI*E

	C_DER = [[C_DER0],[C_DER1],[C_DER2]]

END


;Inverse model on single pixel level
;Input: signals S for a single pixel
;Returns: fitted parameters P

FUNCTION MoCoModel_T1MapMOLLI::PIXEL_PARAMETERS, S, FIT=F

       ExpectedT1 = max(*self.fixed)/4.0

       P = [max(S), 2.0, 1/ExpectedT1] ;[Sinf, Sratio(B/A), R1] @ Sirisha

	   F = mpcurvefit(*self.fixed, S, 1+0E*S, P, function_name='MoCoModel_T1mapMOLLI__Model',/quiet,NODERIVATIVE=1)

;    parinfo = replicate({limited:[0,0], limits:[0,0]}, 3)
;
;   ; P = [max(S),(max(S)-min(S)),1500.0] ; [687.0, 1329.0, 1500.0]
;
;    F = mpcurvefit(*self.fixed, S,  1+0E*S, P, $
;     function_name='MoCoModel_T1mapMOLLI__Model',/quiet,PARINFO=parinfo, /NODERIVATIVE)  ; mpcurvefit (x,y,weights, fitted params; ...)
;

  IF ARG_PRESENT(F) THEN F = self->PIXEL_FORWARD(P)

  RETURN, P

END


;Forward model on single-pixel level
;Input: Model parameters P = 1D array with parameters for 1 pixel
;Returns: fitted signals S for a single pixel

FUNCTION MoCoModel_T1mapMOLLI::PIXEL_FORWARD, P, S

  MoCoModel_T1mapMOLLI__Model, *self.fixed, P, S
  RETURN, S
END



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		CLEANUP, INIT and DEFINE  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



PRO MoCoModel_T1mapMOLLI::CLEANUP

  ptr_free, self.fixed
END



FUNCTION MoCoModel_T1mapMOLLI::INIT, f

  self.nr_of_free_parameters = 3 ; number of free params
  self.fixed = ptr_new(f.TI) ;  ; fixed param

  RETURN, 1
END



PRO MoCoModel_T1mapMOLLI__DEFINE

  struct = {$
    MoCoModel_T1mapMOLLI, $
    INHERITS MoCoModel, $
    fixed: ptr_new() $
  }

END





