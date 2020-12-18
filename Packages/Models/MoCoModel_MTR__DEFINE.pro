

;Inverse model on single pixel level
;Input: signals S for a single pixel
;Returns: fitted parameters P

FUNCTION MoCoModel_MTR::PIXEL_PARAMETERS, S, FIT=F

   P = 100*((S[0,*]-S[1,*])/S[0,*]) ;100*((MT_OFF-MT_ON)/MT_OFF))

   IF ARG_PRESENT(F) THEN F = self->PIXEL_FORWARD(P,S)

 RETURN, P

END


;Forward model on single-pixel level
;Input: Model parameters P = 1D array with parameters for 1 pixel
;Returns: fitted signals S for a single pixel

FUNCTION MoCoModel_MTR::PIXEL_FORWARD, P, S

  RETURN, S
END



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		CLEANUP, INIT and DEFINE  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



FUNCTION MoCoModel_MTR::INIT, f

  self.nr_of_free_parameters = 1
  RETURN, 1
END



PRO MoCoModel_MTR__DEFINE

  struct = {$
    MoCoModel_MTR, $
    INHERITS MoCoModel $
  }

END
