FUNCTION MOCO_2D_DCE_1CMD::FIT_CONC, ct, delay

    IF norm(ct) EQ 0. THEN RETURN, ct

	X = *self.Independent
	n = (n_elements(X)-1)/2
	t = X[0:n-1]
 	ca = ShiftAif(X[n:2*n-1],t,Delay)

    dt = (t[1:n-1]-t[0:n-2])/2E
    ca1 = [0, TOTAL( dt*(ca[0:n-2]+ca[1:n-1]), /cumulative)]
    ct1 = [0, TOTAL( dt*(ct[0:n-2]+ct[1:n-1]), /cumulative)]

	A = TRANSPOSE([[ct1],[ca1]])
	SVDC, A, W, U, V
	X = TRANSPOSE(U) ## TRANSPOSE([ct])
	kpos = where(W GT 0, npos, COMPLEMENT=kzero, NCOMPLEMENT=nzero)
	if npos GT 0 then X[kpos] /= W[kpos]
	if nzero GT 0 then X[kzero] = 0

	RETURN, A ## (V ## X)

END

PRO MOCO_2D_DCE_1CMD::PIXEL_FIT, r

	dDelay = 0.25 ;sec
	maxDelay = 15.0 ;sec
	nDelay = 1+ceil(maxDelay/dDelay)
	Delay = dDelay*findgen(nDelay)

	S = REFORM((*self.Deformed)[*,r[0],r[1]])

	X = *self.Independent
    n0 = X[n_elements(X)-1]
    S0 = total(S[0:n0-1])/n0
    C = S - S0

    Error = fltarr(nDelay)
    for i=0L, nDelay-1 do begin
    	Cfit = self->FIT_CONC(C, Delay[i])
    	Error[i] = total((Cfit-C)^2)
    endfor
    tmp = min(Error,i)
	Cfit = self->FIT_CONC(C, Delay[i])

	Sfit = S0 + Cfit

    (*self.Deformed)[*,r[0],r[1]] = Sfit

END

PRO MOCO_2D_DCE_1CMD::PIXEL_FIT_PRECOMPUTE

END

FUNCTION MOCO_2D_DCE_1CMD::ffd, D

  Di = INTERPOLATE(D, *self.xc, *self.yc, /GRID)

  x = REFORM(Di[0,*,*],/OVERWRITE)
  y = REFORM(Di[1,*,*],/OVERWRITE)

  return, INTERPOLATE(*self.S, x, y)

END

FUNCTION MOCO_2D_DCE_1CMD::FFD_GRAD

  Di = INTERPOLATE(*self.F, *self.xc, *self.yc, /GRID)

  x = REFORM(Di[0,*,*],/OVERWRITE)
  y = REFORM(Di[1,*,*],/OVERWRITE)

  *self.D = INTERPOLATE(*self.S, x, y)

  Res = *self.T - *self.D

  Dx = Res*INTERPOLATE(*self.Sx, long(x), y)
  Dy = Res*INTERPOLATE(*self.Sy, x, long(y))

  Dx = (*self.Weight)*Dx[*self.Weight_loc]
  Dy = (*self.Weight)*Dy[*self.Weight_loc]

  n = Product(self.nb)
  Gradient = FLTARR(2, n, /nozero)

  i0=0L
  FOR i=0L,n-1 DO BEGIN
    i1 = i0 + (*self.Weight_cnt)[i] - 1
    Gradient[0,i] = TOTAL(Dx[i0:i1])
	Gradient[1,i] = TOTAL(Dy[i0:i1])
	i0 = i1 + 1
  ENDFOR

  return, REFORM(Gradient, [2,self.nb], /OVERWRITE)

END

FUNCTION MOCO_2D_DCE_1CMD::FFD_LSEARCH

  scale_down = 1.5
  babystep = 0.1
  stepsize_max = 100.;emergency stop

  Grad = self->ffd_grad()
  Grad /= sqrt(max(total(Grad^2,1)))
  ChiSq_curr = total((*self.T - *self.D)^2.)

  ;backtrack in big steps until reduction in ChiSq

  WHILE 1 DO BEGIN

	F_try = *self.F + self.stepsize*Grad
	D_try = self->ffd(F_try)
	ChiSq_try = total((*self.T-D_try)^2.)

	IF ChiSq_try LT ChiSq_curr THEN BREAK
	IF self.stepsize LT self.precision THEN RETURN, 1B ;Converged

	self.stepsize /= scale_down

  ENDWHILE

  ;forward in baby steps until increase in chisq

  Grad *= babystep

  WHILE 1 DO BEGIN

    *self.F = F_try
    *self.D = D_try
    ChiSq_curr = ChiSq_try

    self.stepsize += babystep
    F_try += Grad
    D_try = self->ffd(F_try)
	ChiSq_try = total((*self.T-D_try)^2.)

    IF ChiSq_try GE ChiSq_curr THEN RETURN, 0B
	IF self.stepsize GT stepsize_max THEN RETURN, 0B ;emergency stop

  ENDWHILE

END


FUNCTION MOCO_2D_DCE_1CMD::FFD_REG, t

 itmax = 100. ;emergency stop
 self.stepsize = 5.0 ;pixels

 *self.S = REFORM((*self.Source)[t,*,*],/overwrite)
 *self.T = REFORM((*self.Deformed)[t,*,*],/overwrite)
 *self.F = REFORM((*self.DefField)[t,*,*,*],/overwrite)
 *self.sx = (*self.S)[1:self.ns[1]-1,*] - (*self.S)[0:self.ns[1]-2,*]
 *self.sy = (*self.S)[*,1:self.ns[2]-1] - (*self.S)[*,0:self.ns[2]-2]

 FOR iterations=1L, itmax DO $
   IF self->ffd_lsearch() THEN BREAK

 (*self.Deformed)[t,*,*] = *self.D
 (*self.DefField)[t,*,*,*] = *self.F

 return, iterations EQ 1B
END


PRO MOCO_2D_DCE_1CMD::ffd_reg_precompute

  ds = (self.nb-1E)/(self.nw-1E)

  *self.xc = ds[0]*findgen(self.nw[0])
  *self.yc = ds[1]*findgen(self.nw[1])

  rp = fltarr([2,self.nw])
  FOR i=0L, self.nw[0]-1 DO BEGIN
  FOR j=0L, self.nw[1]-1 DO BEGIN
    rp[0,i,j] = ds[0]*i
    rp[1,i,j] = ds[1]*j
  ENDFOR
  ENDFOR
  rp = REFORM(rp, 2, product(self.nw), /overwrite)

  n_b = product(self.nb)
  n_s = product(self.nw)

  *self.Weight_cnt = lonarr(n_b)
  *self.Weight_loc = lonarr(4*n_s)
  *self.Weight = fltarr(4*n_s)

  Func = fltarr(3,3)
  Func[1,1] = 1

  i0 = 0L
  rb = array_indices(self.nb, lindgen(n_b), /dimensions)
  FOR i=0L,n_b-1 DO BEGIN

    ui = rp[0,*]-rb[0,i]
    vi = rp[1,*]-rb[1,i]

  	Wi_loc = where((abs(ui) lt 1) and (abs(vi) lt 1), Wi_cnt)
  	Wi = INTERPOLATE(Func, 1+ui[Wi_loc], 1+vi[Wi_loc])

    i1 = i0 + Wi_cnt - 1
	(*self.Weight_cnt)[i] = Wi_cnt
	(*self.Weight_loc)[i0:i1] = Wi_loc
	(*self.Weight)[i0:i1] = Wi
	i0 = i1 + 1
  ENDFOR
END

PRO MOCO_2D_DCE_1CMD::PKREG

  iter = 100 ;emergency stop

  self -> ffd_reg_precompute

  nPixels = Product(self.nw)
  FOR it=1L,iter DO BEGIN

    FOR i=0L,nPixels-1 DO BEGIN
      r = array_indices(self.nw, i, /dimensions)
      self -> PIXEL_FIT, r
    ENDFOR

    converged = 1B
    FOR t=0L,self.ns[0]-1 DO converged *= self->ffd_reg(t)

    IF converged THEN BREAK

  ENDFOR

END


PRO MOCO_2D_DCE_1CMD::SCALE_UP

  nold = size(*self.DefField, /dimensions)
  dr = (nold[2:*]-1E)/(self.nb-1E)

  X = dr[0] * findgen(self.nb[0])
  Y = dr[1] * findgen(self.nb[1])

  *self.DefField = INTERPOLATE(*self.DefField, X, Y, /GRID)

END

PRO MOCO_2D_DCE_1CMD::INITIALISE

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

FUNCTION MOCO_2D_DCE_1CMD::DEFORMED

  self -> PIXEL_FIT_PRECOMPUTE

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
  self -> initialise
  self -> pkreg

  FOR it=1L,Order-1 DO BEGIN
  	  self.nB = 1 + ceil(FOVnorm*nCells[it])
  	  self -> scale_up
  	  self -> pkreg
  ENDFOR

  (*self.Source)[*, xw, yw] = *self.Deformed

  return, *self.Source
END

PRO MOCO_2D_DCE_1CMD::CLEANUP

  ptr_free, $
    self.source, $
    self.independent, $
    self.DefField, $
    self.Deformed, $
    self.S, $
    self.T, $
    self.F, $
    self.D, $
    self.Sx, $
    self.Sy, $
    self.xc, $
    self.yc, $
    self.Weight, $
    self.Weight_loc, $
    self.Weight_cnt

END

FUNCTION MOCO_2D_DCE_1CMD::INIT, $
  Source, $
  Resolution, $
  Precision, $
  Independent, $
  WIN=Win

  self.nS = size(*source, /Dimensions)
  self.source = source
  self.independent = ptr_new(independent)
  self.DefField = ptr_new(/allocate_heap)
  self.Deformed = ptr_new(/allocate_heap)
  self.S = ptr_new(/allocate_heap)
  self.T = ptr_new(/allocate_heap)
  self.F = ptr_new(/allocate_heap)
  self.D = ptr_new(/allocate_heap)
  self.sx = ptr_new(/allocate_heap)
  self.sy = ptr_new(/allocate_heap)
  self.xc = ptr_new(/allocate_heap)
  self.yc = ptr_new(/allocate_heap)
  self.Weight = ptr_new(/allocate_heap)
  self.Weight_cnt = ptr_new(/allocate_heap)
  self.Weight_loc = ptr_new(/allocate_heap)
  self.stepsize = 0E
  self.resolution = resolution
  self.precision = precision
  if n_elements(Win) ne 0 then begin
    self.Win_p = Win.p
    self.nw = Win.n
  endif else self.nw = self.ns[1:*]

  return, 1
END

PRO MOCO_2D_DCE_1CMD__DEFINE

  struct = {MOCO_2D_DCE_1CMD, $
  	source: ptr_new(), $
  	Independent: ptr_new(), $
  	DefField: ptr_new(), $
  	Deformed: ptr_new(), $
  	S: ptr_new(), $
  	T: ptr_new(), $
  	F: ptr_new(), $
  	D: ptr_new(), $
  	Sx: ptr_new(), $
  	Sy: ptr_new(), $
  	xc: ptr_new(), $
  	yc: ptr_new(), $
  	Weight: ptr_new(), $
  	Weight_cnt: ptr_new(), $
  	Weight_loc: ptr_new(), $
  	stepsize: 0E, $
  	Resolution: 0E, $
  	Precision: 0E, $
  	Win_p:lonarr(3), $
  	ns: lonarr(3), $
  	nb: lonarr(2), $
  	nw: lonarr(2) }
END