

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		OVERRIDES                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



FUNCTION MoCoModel_Constant::PIXEL_RESIDUE_PDER, P, S, k

  S_k = S*0
  S_k[k] = 1
  RETURN, S_k - 1E/N_ELEMENTS(S)
END




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		REQUIRED                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



FUNCTION MoCoModel_Constant::PIXEL_PARAMETERS, S, FIT=F

  P = TOTAL(S)/FLOAT(N_ELEMENTS(S))
  IF ARG_PRESENT(F) THEN F = self->PIXEL_FORWARD(P,S)
  RETURN, P
END


FUNCTION MoCoModel_Constant::PIXEL_FORWARD, P, S

  RETURN, P + S*0
END



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		CLEANUP, INIT and DEFINE  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



PRO MoCoModel_Constant::CLEANUP
END


FUNCTION MoCoModel_Constant::INIT, fixed_parameters

  self.nr_of_free_parameters = 1

  RETURN, 1
END


PRO MoCoModel_Constant__DEFINE

  struct = {$
    MoCoModel_Constant, $
    INHERITS MoCoModel $
  }
END