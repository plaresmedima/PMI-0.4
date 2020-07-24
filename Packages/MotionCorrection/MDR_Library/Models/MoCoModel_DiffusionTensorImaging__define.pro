

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		REQUIRED                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Inverse model on single pixel level
;Input: signals S for a single pixel
;Returns: fitted parameters P

FUNCTION MoCoModel_DiffusionTensorImaging::PIXEL_PARAMETERS, S, FIT=F

  ;Tale the ln of the signal
  lnS = S
  pos = WHERE(S GT 0, npos)
  IF npos GT 0 THEN lnS[pos] = ALOG(S[pos])

  ;Multiply the ln with the inverse matrix
  P = (*self.matrix_inverse) ## lnS

  ;Force all D-tensor components to be positive
  P[1:*] = P[1:*] > 0

  IF ARG_PRESENT(F) THEN F = self->PIXEL_FORWARD(P)

  RETURN, P

END


;Forward model on single-pixel level
;Input:
;	DTI model parameters P = 1D array with DTI parameters for 1 pixel
;	P[0] = lnS0
;	P[1] = Dxx etc
;Returns:
;	fitted signals S for a single pixel

FUNCTION MoCoModel_DiffusionTensorImaging::PIXEL_FORWARD, P, S

  RETURN, EXP((*self.matrix_forward) ## P)

END



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		CLEANUP, INIT and DEFINE  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



PRO MoCoModel_DiffusionTensorImaging::CLEANUP

  ptr_free, self.matrix_forward, self.matrix_inverse
END


FUNCTION MoCoModel_DiffusionTensorImaging::INIT, fixed_parameters

  b = fixed_parameters.b
  g = fixed_parameters.g

  np = 7
  nb = n_elements(b)

  self.nr_of_free_parameters = np

  matrix = FLTARR(np,nb)
  matrix[0,*] = 1 ;lnS0
  matrix[1,*] = -b*g[0,*]*g[0,*] ;Dxx
  matrix[2,*] = -b*g[1,*]*g[1,*] ;Dyy
  matrix[3,*] = -b*g[2,*]*g[2,*] ;Dzz
  matrix[4,*] = -2*b*g[0,*]*g[1,*] ;Dxy
  matrix[5,*] = -2*b*g[1,*]*g[2,*] ;Dyz
  matrix[6,*] = -2*b*g[2,*]*g[0,*] ;Dzx

  self.matrix_forward = ptr_new(matrix)

  SVDC, matrix, W, U, V
  U = TRANSPOSE(U)
  FOR i=0L,np-1 DO BEGIN
    sv_inv = 0
    IF W[i] GT 0 THEN sv_inv = 1E/W[i]
    U[*,i] *= sv_inv
  ENDFOR
  matrix = V ## U

  self.matrix_inverse = ptr_new(matrix)

  RETURN, 1
END



PRO MoCoModel_DiffusionTensorImaging__DEFINE

  struct = {$
    MoCoModel_DiffusionTensorImaging, $
    INHERITS MoCoModel, $
    matrix_forward: ptr_new(), $
    matrix_inverse: ptr_new() $
  }

END