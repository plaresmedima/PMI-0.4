FUNCTION MoCoModel::PIXEL_RESIDUE_PDER, P, S, k

  epsilon = 0.001*max(S)
  if epsilon eq 0 then epsilon = 0.001

  S[k] += epsilon
  F = self->PIXEL_FORWARD(P,S)
  S[k] -= epsilon
  F -= self->PIXEL_FORWARD(P,S)

  RETURN, F/epsilon
END




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		PUBLIC METHODS			  ;;
;;		WHOLE IMAGE OPERATIONS    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



FUNCTION MoCoModel::RESIDUE_PDER, P, S, k

  n = SIZE(S, /Dimensions)
  npixels = PRODUCT(n[1:*])

  P = REFORM(P, self.nr_of_free_parameters, npixels, /OVERWRITE)
  S = REFORM(S, n[0], npixels, /OVERWRITE)
  R = S

  FOR i=0L,npixels-1 DO BEGIN
    Pi = REFORM(P[*,i])
    Si = REFORM(S[*,i])
    R[*,i] = self -> PIXEL_RESIDUE_PDER(Pi, Si, k)
  ENDFOR

  P = REFORM(P, [self.nr_of_free_parameters,n[1:*]], /OVERWRITE)
  S = REFORM(S, n, /OVERWRITE)
  R = REFORM(R, n, /OVERWRITE)

  RETURN, R
END


PRO MoCoModel::PARAMETERS, S, P

  n = SIZE(S, /Dimensions)
  npixels = PRODUCT(n[1:*])

  IF N_ELEMENTS(P) EQ 0 THEN P = FLTARR(self.nr_of_free_parameters, npixels) $
  ELSE P = REFORM(P, self.nr_of_free_parameters, npixels, /OVERWRITE)
  S = REFORM(S, n[0], npixels, /OVERWRITE)

  FOR i=0L,npixels-1 DO BEGIN
    Si = REFORM(S[*,i])
    P[*,i] = self -> PIXEL_PARAMETERS(Si)
  ENDFOR

  P = REFORM(P, [self.nr_of_free_parameters,n[1:*]], /OVERWRITE)
  S = REFORM(S, n, /OVERWRITE)

END


FUNCTION MoCoModel::CHISQ, P, S

;  R = self->RESIDUE(P,S)
;  RETURN, 0.5*TOTAL(R^2)

  n = SIZE(S, /Dimensions)
  npixels = PRODUCT(n[1:*])

  P = REFORM(P, self.nr_of_free_parameters, npixels, /OVERWRITE)
  S = REFORM(S, n[0], npixels, /OVERWRITE)

  ChiSq = 0E
  FOR i=0L,npixels-1 DO BEGIN
    Pi = REFORM(P[*,i])
    Si = REFORM(S[*,i])
    Si = Si - self->PIXEL_FORWARD(Pi, Si)
    ChiSq += TOTAL(Si^2)
  ENDFOR

  P = REFORM(P, [self.nr_of_free_parameters,n[1:*]], /OVERWRITE)
  S = REFORM(S, n, /OVERWRITE)

  return, 0.5*ChiSq
END



FUNCTION MoCoModel::GROUP_RESIDUE, P, S

  n = SIZE(S, /Dimensions)

  Gres = FLTARR(n[0], PRODUCT(n[1:*]))

  self -> RESIDUE, S, P, Res
  FOR k=0L,n[0]-1 DO BEGIN
    Res_k = Self->RESIDUE_PDER(P, S, k)
    Gres[k,*] = TOTAL(Res * Res_k, 1)
  ENDFOR

  Gres = REFORM(Gres, n, /OVERWRITE)

  RETURN, Gres

END



PRO MoCoModel::FORWARD, S, P, F

  n = SIZE(S, /Dimensions)
  npixels = PRODUCT(n[1:*])

  P = REFORM(P, self.nr_of_free_parameters, npixels, /OVERWRITE)
  S = REFORM(S, n[0], npixels, /OVERWRITE)
  IF N_ELEMENTS(F) EQ 0 THEN F = S $
  ELSE F = REFORM(F, n[0], npixels, /OVERWRITE)

  FOR i=0L,npixels-1 DO BEGIN
    Pi = REFORM(P[*,i])
    Si = REFORM(S[*,i])
    F[*,i] = self->PIXEL_FORWARD(Pi, Si)
  ENDFOR

  P = REFORM(P, [self.nr_of_free_parameters,n[1:*]], /OVERWRITE)
  S = REFORM(S, n, /OVERWRITE)
  F = REFORM(F, n, /OVERWRITE)

END

PRO MoCoModel::RESIDUE, S, P, R

  self -> FORWARD, S, P, R
  R = S - R
END


;PRO MoCoModel::FIT, S, F
;
;  self -> PARAMETERS, S, P
;  self -> FORWARD, S, P, F
;END

PRO MoCoModel::FIT, S, F

  n = SIZE(S, /Dimensions)
  npixels = PRODUCT(n[1:*])

  S = REFORM(S, n[0], npixels, /OVERWRITE)
  IF N_ELEMENTS(F) EQ 0 THEN F = S $
  ELSE F = REFORM(F, n[0], npixels, /OVERWRITE)

  FOR i=0L,npixels-1 DO BEGIN
    Si = REFORM(S[*,i])
    Pi = self -> PIXEL_PARAMETERS(Si, FIT=Fi)
    F[*,i] = Fi
  ENDFOR

  S = REFORM(S, n, /OVERWRITE)
  F = REFORM(F, n, /OVERWRITE)

END



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		CLEANUP, INIT and DEFINE  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




PRO MoCoModel::CLEANUP
END


FUNCTION MoCoModel::INIT, fixed_parameters

  self.nr_of_free_parameters = 1
  RETURN, 1
END


PRO MoCoModel__DEFINE

  struct = {$
    MoCoModel, $
    nr_of_free_parameters: 1 $
  }

END