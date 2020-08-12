





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		REQUIRED                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


FUNCTION MoCoModel_DCEModelFree::PIXEL_PARAMETERS, S, FIT=F

  n = n_elements(S)

  S0 = total(S[0:self.n0-1])/self.n0
  ct = S - S0
  IF self.interp THEN ct = INTERPOL(ct, *self.time, *self.time_regr)

  P = (*self.matrix_inverse) ## ct

  IF ARG_PRESENT(F) THEN F = self->PIXEL_FORWARD(P, S)

  RETURN, P

END



FUNCTION MoCoModel_DCEModelFree::PIXEL_FORWARD, P, S

  n = n_elements(S)
  S0 = total(S[0:self.n0-1])/self.n0

  F = S0 + (*self.matrix_forward) ## P

  IF self.interp THEN F = INTERPOL(F, *self.time_regr, *self.time)

  RETURN, F

END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		CLEANUP, INIT and DEFINE  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



PRO MoCoModel_DCEModelFree::CLEANUP

  ptr_free $
  , self.matrix_forward $
  , self.matrix_inverse $
  , self.time $
  , self.time_regrid

END


FUNCTION MoCoModel_DCEModelFree::INIT, fixed_parameters

  self.interp = RegridDeconvolutionData($
  	fixed_parameters.t, fixed_parameters.ca, $
  	t, ca)

  self.nr_of_free_parameters = n_elements(t)

  mat = (t[1]-t[0])*convolution_matrix(ca)
  svdc, mat, W,U,V, /double

  zero = where(W lt 0.15*max(W),nzero)
  W = 1/W
  if nzero gt 0 then W[zero]=0

  X = transpose(U)
  n = size(X,/DIMENSIONS)
  for i=0L,n[1]-1 do X[*,i] *= W[i]
  mat_inv = V ## X

  self.n0 = fixed_parameters.n0
  self.matrix_forward = ptr_new(mat)
  self.matrix_inverse = ptr_new(mat_inv)
  self.time = ptr_new(fixed_parameters.t)
  self.time_regrid = ptr_new(t)

  RETURN, 1
END



PRO MoCoModel_DCEModelFree__DEFINE

  struct = {$
    MoCoModel_DCEModelFree, $
    INHERITS MoCoModel, $
    n0: 0L, $
    interp: 0B, $
    time: ptr_new(), $
    time_regrid: ptr_new(), $
    matrix_forward: ptr_new(), $
    matrix_inverse: ptr_new() $
  }

END