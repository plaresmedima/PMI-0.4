PRO MOCOMO_2D::OPTIMISE_MODEL_TARGET, i

  r = array_indices(self.nw, i, /dimensions)
  S = REFORM((*self.Deformed)[*,r[0],r[1]])
  S = self->FIT_SIGNAL(S)
  (*self.Deformed)[*,r[0],r[1]] = S

END


FUNCTION MOCOMO_2D::OPTIMISE_DEFORMATION_FIELD, t

 converged = self -> COREGISTER(self.ns[1:*], $
 	REFORM((*self.Source)[t,*,*],/overwrite), $
 	REFORM((*self.Deformed)[t,*,*],/overwrite), $
 	REFORM((*self.DefField)[t,*,*,*],/overwrite) )

 (*self.Deformed)[t,*,*] = *self.D
 (*self.DefField)[t,*,*,*] = *self.F

 return, converged

END


PRO MOCOMO_2D::JOINT_OPTIMISATION

  iter = 100 ;emergency stop

  self -> CALCULATE_INTERPOLATION_WEIGHTS

  nPixels = Product(self.nw)
  FOR it=1L,iter DO BEGIN

    FOR i=0L,nPixels-1 DO $
      self -> OPTIMISE_MODEL_TARGET, i

    converged = 1B
    FOR t=0L,self.ns[0]-1 DO $
      converged *= self -> OPTIMISE_DEFORMATION_FIELD(t)

    IF converged THEN BREAK

  ENDFOR

END


PRO MOCOMO_2D::SCALE_UP_DEFORMATION_FIELDS

  nold = size(*self.DefField, /dimensions)
  dr = (nold[2:*]-1E)/(self.nb-1E)

  X = dr[0] * findgen(self.nb[0])
  Y = dr[1] * findgen(self.nb[1])

  *self.DefField = INTERPOLATE(*self.DefField, X, Y, /GRID)

END

PRO MOCOMO_2D::INITIALISE_DEFORMATION_FIELDS

;Define D(x,k) = x

  db = (self.nw-1E)/(self.nb-1E)
  rb = fltarr([2,self.nb])

  FOR i=0L, self.nb[0]-1 DO BEGIN
  FOR j=0L, self.nb[1]-1 DO BEGIN
    rb[0,i,j] = self.Win_p[0] + db[0]*i
    rb[1,i,j] = self.Win_p[1] + db[1]*j
  ENDFOR
  ENDFOR

  P = fltarr([self.ns[0],2,self.nb])
  for t=0L,self.ns[0]-1 do P[t,*,*,*] = rb

  *self.DefField = P

END

FUNCTION MOCOMO_2D::DEFORMED

  xw = self.Win_p[0] + lindgen(self.nw[0])
  yw = self.Win_p[1] + lindgen(self.nw[1])

  *self.Deformed = (*self.Source)[*, xw, yw]

  nCells = ceil(float(max(self.nw))/self.Resolution)
  Order = 0L
  While 2L^Order LT nCells DO Order += 1
  nCells = 2L^lindgen(Order)
  FOVnorm = float(self.nw)/max(self.nw)

  it = 0L
  self.nB = 1 + ceil(FOVnorm*nCells[it])
  self -> initialise_deformation_fields
  self -> JOINT_OPTIMISATION

  FOR it=1L,Order-1 DO BEGIN
  	  self.nB = 1 + ceil(FOVnorm*nCells[it])
  	  self -> scale_up_deformation_fields
  	  self -> JOINT_OPTIMISATION
  ENDFOR

  (*self.Source)[*, xw, yw] = *self.Deformed

  return, *self.Source
END


PRO MOCOMO_2D::CLEANUP

  ptr_free, $
    self.independent, $
    self.source, $
    self.DefField, $
    self.Deformed

  self -> COREG_2D_FFD_LS::CLEANUP
END

FUNCTION MOCOMO_2D::INIT, $
  Source, $
  Resolution, $
  Precision, $
  Independent, $
  WIN=Win

  self -> SET_MODEL, Independent

  self.nS = size(*source, /Dimensions)
  self.source = source
  self.DefField = ptr_new(/allocate_heap)
  self.Deformed = ptr_new(/allocate_heap)
  self.resolution = resolution

  if n_elements(Win) ne 0 then self.Win_p = Win.p
  if n_elements(Win) ne 0 then nw = Win.n else nw = self.ns[1:*]

  ok = self -> COREG_2D_FFD_LS::INIT(Precision, nw)

  return, 1
END

PRO MOCOMO_2D__DEFINE

  struct = {MOCOMO_2D, $
    Independent: ptr_new(), $
  	ns: lonarr(3), $
  	source: ptr_new(), $
  	DefField: ptr_new(), $
  	Deformed: ptr_new(), $
   	Win_p:lonarr(3), $
   	Resolution: 0E, $
  	INHERITS COREG_2D_FFD_LS  }
END