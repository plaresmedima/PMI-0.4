


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		OVERRIDES                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



FUNCTION MoCoModel_TwoCompartmentFiltration::PIXEL_RESIDUE_PDER, P, S, k
;Verified numerically

  S_k = S*0
  S_k[k] = 1

  S0 = total(S_k[0:self.n0-1])/self.n0
  ct = S_k - S0

  dt = *self.dt
  n = n_elements(S)
  ct1 = [0, TOTAL( dt*(ct[0:n-2]+ct[1:n-1]) , /cumulative)]
  ct2 = [0, TOTAL( dt*(ct1[0:n-2]+ct1[1:n-1]) , /cumulative)]

  RETURN, S_k - (S0 - ct2*P[0] - ct1*P[1])

END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		REQUIRED                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


FUNCTION MoCoModel_TwoCompartmentFiltration::PIXEL_PARAMETERS, S, FIT=F

  n = n_elements(S)

  S0 = total(S[0:self.n0-1])/self.n0
  ct = S - S0

  IF norm(ct) EQ 0. THEN BEGIN
    IF ARG_PRESENT(F) THEN F = S*0
    RETURN, fltarr(4)
  ENDIF

  dt = *self.dt
  ct1 = [0, TOTAL( dt*(ct[0:n-2]+ct[1:n-1]) , /cumulative)]
  ct2 = [0, TOTAL( dt*(ct1[0:n-2]+ct1[1:n-1]) , /cumulative)]

  (*self.matrix)[0,*] = -ct2
  (*self.matrix)[1,*] = -ct1

  SVDC, *self.matrix, W, U, V
  Y = TRANSPOSE(U) ## TRANSPOSE([ct])
  kpos = where(W GT 0, npos, COMPLEMENT=kzero, NCOMPLEMENT=nzero)
  if npos GT 0 then Y[kpos] /= W[kpos]
  if nzero GT 0 then Y[kzero] = 0

  P = V ## Y

  IF ARG_PRESENT(F) THEN F = S0 + (*self.matrix) ## P

  RETURN, P

END


FUNCTION MoCoModel_TwoCompartmentFiltration::PIXEL_FORWARD, P, S

  n = n_elements(S)
  S0 = total(S[0:self.n0-1])/self.n0
  ct = S - S0

  dt = *self.dt
  ct1 = [0, TOTAL( dt*(ct[0:n-2]+ct[1:n-1]) , /cumulative)]
  ct2 = [0, TOTAL( dt*(ct1[0:n-2]+ct1[1:n-1]) , /cumulative)]

  (*self.matrix)[0,*] = -ct2
  (*self.matrix)[1,*] = -ct1

  RETURN, S0 + (*self.matrix) ## P

END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		CLEANUP, INIT and DEFINE  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



PRO MoCoModel_TwoCompartmentFiltration::CLEANUP

  ptr_free, self.dt, self.matrix
END


FUNCTION MoCoModel_TwoCompartmentFiltration::INIT, fixed_parameters

  self.nr_of_free_parameters = 4

  t = fixed_parameters.t
  ca = fixed_parameters.ca

  n = n_elements(t)
  dt = (t[1:n-1]-t[0:n-2])/2E
  ca1 = [0, TOTAL( dt*(ca[0:n-2]+ca[1:n-1]) , /cumulative)]
  ca2 = [0, TOTAL( dt*(ca1[0:n-2]+ca1[1:n-1]) , /cumulative)]

  matrix = TRANSPOSE([[-ca2],[-ca1],[ca1],[ca2]])

  self.n0 = fixed_parameters.n0
  self.dt = ptr_new(dt)
  self.matrix = ptr_new(matrix)

  RETURN, 1
END


PRO MoCoModel_TwoCompartmentFiltration__DEFINE

  struct = {$
    MoCoModel_TwoCompartmentFiltration, $
    INHERITS MoCoModel, $
    n0: 0L, $
    dt: ptr_new(), $
    matrix: ptr_new() $
  }

END