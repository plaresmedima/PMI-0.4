FUNCTION COREG_3D_FFD_LS::DEF, D

  Di = INTERPOLATE(D, *self.xc, *self.yc, *self.zc, /GRID)

  x = REFORM(Di[0,*,*,*],/OVERWRITE)
  y = REFORM(Di[1,*,*,*],/OVERWRITE)
  z = REFORM(Di[2,*,*,*],/OVERWRITE)

  return, INTERPOLATE(*self.S, x, y, z)

END

FUNCTION COREG_3D_FFD_LS::GRAD

  Di = INTERPOLATE(*self.F, *self.xc, *self.yc, *self.zc, /GRID)

  x = REFORM(Di[0,*,*,*],/OVERWRITE)
  y = REFORM(Di[1,*,*,*],/OVERWRITE)
  z = REFORM(Di[2,*,*,*],/OVERWRITE)

  *self.D = INTERPOLATE(*self.S, x, y, z)

  Res = *self.T - *self.D

  Dx = Res*INTERPOLATE(*self.Sx, long(x), y, z)
  Dy = Res*INTERPOLATE(*self.Sy, x, long(y), z)
  Dz = Res*INTERPOLATE(*self.Sz, x, y, long(z))

  Dx = (*self.Weight)*Dx[*self.Weight_loc]
  Dy = (*self.Weight)*Dy[*self.Weight_loc]
  Dz = (*self.Weight)*Dz[*self.Weight_loc]

  n = Product(self.nb)
  Gradient = FLTARR(3, n, /nozero)

  i0=0L
  FOR i=0L,n-1 DO BEGIN
    i1 = i0 + (*self.Weight_cnt)[i] - 1
    Gradient[0,i] = TOTAL(Dx[i0:i1])
	Gradient[1,i] = TOTAL(Dy[i0:i1])
	Gradient[2,i] = TOTAL(Dz[i0:i1])
	i0 = i1 + 1
  ENDFOR

  return, REFORM(Gradient, [3,self.nb], /OVERWRITE)

END

FUNCTION COREG_3D_FFD_LS::LSEARCH

  scale_down = 1.5
  babystep = 0.1
  stepsize_max = 100.;emergency stop

  Grad = self->GRAD()
  Grad /= sqrt(max(total(Grad^2,1)))
  ChiSq_curr = total((*self.T - *self.D)^2.)

  ;backtrack in big steps until reduction in ChiSq

  WHILE 1 DO BEGIN

	F_try = *self.F + self.stepsize*Grad
	D_try = self->DEF(F_try)
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
    D_try = self->DEF(F_try)
	ChiSq_try = total((*self.T-D_try)^2.)

    IF ChiSq_try GE ChiSq_curr THEN RETURN, 0B
	IF self.stepsize GT stepsize_max THEN RETURN, 0B ;emergency stop

  ENDWHILE

END


FUNCTION COREG_3D_FFD_LS::SOLVE, nS, S, T, F

 itmax = 100. ;emergency stop
 self.stepsize = 5.0 ;pixels

 *self.S = S
 *self.T = T
 *self.F = F

 *self.Sx = S[1:ns[0]-1,*,*] - S[0:ns[0]-2,*,*]
 *self.Sy = S[*,1:ns[1]-1,*] - S[*,0:ns[1]-2,*]
 *self.Sz = S[*,1:ns[1]-1,*] - S[*,0:ns[1]-2,*]

 FOR iterations=1L, itmax DO $
   IF self->LSEARCH() THEN BREAK

 return, iterations EQ 1B

END

PRO COREG_3D_FFD_LS::PRECOMPUTE

  ds = (self.nb-1E)/(self.nw-1E)

  *self.xc = ds[0]*findgen(self.nw[0])
  *self.yc = ds[1]*findgen(self.nw[1])
  *self.zc = ds[2]*findgen(self.nw[2])

  rp = fltarr([3,self.nw])
  FOR i=0L, self.nw[0]-1 DO BEGIN
  FOR j=0L, self.nw[1]-1 DO BEGIN
  FOR k=0L, self.nw[2]-1 DO BEGIN
    rp[0,i,j,k] = ds[0]*i
    rp[1,i,j,k] = ds[1]*j
    rp[2,i,j,k] = ds[2]*k
  ENDFOR
  ENDFOR
  ENDFOR
  rp = REFORM(rp, 3, product(self.nw), /overwrite)

  n_b = product(self.nb)
  n_s = product(self.nw)

  *self.Weight_cnt = lonarr(n_b)
  *self.Weight_loc = lonarr(8*n_s)
  *self.Weight = fltarr(8*n_s)

  Func = fltarr(3,3,3)
  Func[1,1,1] = 1

  i0 = 0L
  rb = array_indices(self.nb, lindgen(n_b), /dimensions)
  FOR i=0L,n_b-1 DO BEGIN

    ui = rp[0,*]-rb[0,i]
    vi = rp[1,*]-rb[1,i]
    wi = rp[2,*]-rb[2,i]

  	Wi_loc = where((abs(ui) lt 1) and (abs(vi) lt 1) and (abs(wi) lt 1), Wi_cnt)
  	Wi = INTERPOLATE(Func, 1+ui[Wi_loc], 1+vi[Wi_loc], 1+wi[Wi_loc])

    i1 = i0 + Wi_cnt - 1
	(*self.Weight_cnt)[i] = Wi_cnt
	(*self.Weight_loc)[i0:i1] = Wi_loc
	(*self.Weight)[i0:i1] = Wi
	i0 = i1 + 1
  ENDFOR
END


PRO COREG_3D_FFD_LS::CLEANUP

  ptr_free, $
    self.S, $
    self.T, $
    self.F, $
    self.D, $
    self.Sx, $
    self.Sy, $
    self.Sz, $
    self.xc, $
    self.yc, $
    self.zc, $
    self.Weight, $
    self.Weight_loc, $
    self.Weight_cnt
END


FUNCTION COREG_3D_FFD_LS::INIT, $
  Precision, nw

  self.S = ptr_new(/allocate_heap)
  self.T = ptr_new(/allocate_heap)
  self.F = ptr_new(/allocate_heap)
  self.D = ptr_new(/allocate_heap)
  self.sx = ptr_new(/allocate_heap)
  self.sy = ptr_new(/allocate_heap)
  self.sz = ptr_new(/allocate_heap)
  self.xc = ptr_new(/allocate_heap)
  self.yc = ptr_new(/allocate_heap)
  self.zc = ptr_new(/allocate_heap)
  self.Weight = ptr_new(/allocate_heap)
  self.Weight_cnt = ptr_new(/allocate_heap)
  self.Weight_loc = ptr_new(/allocate_heap)
  self.stepsize = 0E
  self.precision = precision
  self.nw = nw
  self.nb = nw

  return, 1
END

PRO COREG_3D_FFD_LS__DEFINE

  struct = {COREG_3D_FFD_LS, $
  	S: ptr_new(), $
  	T: ptr_new(), $
  	F: ptr_new(), $
  	D: ptr_new(), $
  	Sx: ptr_new(), $
  	Sy: ptr_new(), $
  	Sz: ptr_new(), $
  	xc: ptr_new(), $
  	yc: ptr_new(), $
  	zc: ptr_new(), $
  	Weight: ptr_new(), $
  	Weight_cnt: ptr_new(), $
  	Weight_loc: ptr_new(), $
  	Stepsize: 0E, $
  	Precision: 0E, $
  	nb: lonarr(3), $
  	nw: lonarr(3) }

END