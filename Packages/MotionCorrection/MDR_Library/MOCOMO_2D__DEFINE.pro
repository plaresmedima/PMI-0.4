FUNCTION MOCOMO_2D::SCALEUP, deformation_field, grid_size

  nf = size(deformation_field, /dimensions)
  dr = (nf[2:*]-1E)/(grid_size-1E)

  X = dr[0] * findgen(grid_size[0])
  Y = dr[1] * findgen(grid_size[1])

  RETURN, INTERPOLATE(deformation_field, X, Y, /GRID)
END



FUNCTION MOCOMO_2D::NODEFORMATION, nt, grid_size

  ndim = 2

  db = (self.win[ndim:2*ndim-1]-1E)/(grid_size-1E)
  rb = fltarr([ndim,grid_size])

  FOR i=0L, grid_size[0]-1 DO BEGIN
  FOR j=0L, grid_size[1]-1 DO BEGIN
    rb[0,i,j] = self.win[0] + db[0]*i
    rb[1,i,j] = self.win[1] + db[1]*j
  ENDFOR
  ENDFOR

  P = fltarr([nt,ndim,grid_size])
  for t=0L,nt-1 do P[t,*,*,*] = rb

  return, P
END



PRO MOCOMO_2D::SET_RESOLUTION_LEVELS

  ndim = 2
  base = 2.0

  nw = self.win[ndim:2*ndim-1]
  nCells = ceil(float(max(nw))/self.resolution)
  Order = 0L
  While base^Order LT nCells DO Order += 1
  nCells = base^lindgen(Order)
  FOVnorm = float(nw)/max(nw)
  nb = lonarr(Order,ndim)
  for i=0L, Order-1 do nb[i,*] = 1 + ceil(FOVnorm*nCells[i])
  self.grid = ptr_new(nb)

END

;Definitions:
; (K, Sx, Sy) the dimensions (columns, rows) of the source image
; (Wx, Wy) the dimensions (columns, rows) of the window within the source image
; (K, Dx, Dy) the dimensions (columns, rows) of the grid of control points

;Inputs:
; S: Source Image, K * Sx * Sy array
; D: Deformation Field, K * 2 * Dx * Dy array, with D[*,0,*,*] = X-component and D[*,1,*,*] = Y-component
; xc: X-coordinates of image pixel centres within window, Wx * Wy array, assuming distance between neighbouring control points = 1
; yc: Y-coordinates of image pixel centres within window, Wx * Wy array, assuming distance between neighbouring control points = 1

PRO MOCOMO_2D::FFD, F, S, D, xc, yc

  n = size(D, /Dimensions)
  IF N_ELEMENTS(F) EQ 0 THEN F = FLTARR(n[0], N_ELEMENTS(xc), N_ELEMENTS(yc), /NOZERO)

  FOR k=0L,n[0]-1 DO BEGIN

    xk = INTERPOLATE(D[k,0,*,*], xc, yc, /GRID)
    yk = INTERPOLATE(D[k,1,*,*], xc, yc, /GRID)

    F[k,*,*] = INTERPOLATE(S[k,*,*], xk, yk)

  ENDFOR
END



FUNCTION MOCOMO_2D::CHISQ, deformed, target

  RETURN, 0.5*total((deformed - Target)^2.)
END



FUNCTION MOCOMO_2D::LINESEARCH, Grad, Source, deformed, deformation_field, target, xc, yc, STEPSIZE=stepsize

  scale_down = 1.5
  babystep = 0.1*self.tolerance
  stepsize_max = 100.;emergency stop

  Grad /= sqrt(max(total(Grad^2,2)))
  ChiSq = self->ChiSq(deformed, target)

  ;backtrack in big steps until reduction in ChiSq

  stepsize_try = stepsize

  WHILE 1 DO BEGIN

	F_try = deformation_field + stepsize_try*Grad
	self->FFD, D_try, Source, F_try, xc, yc
	ChiSq_try = self->ChiSq(D_try, target)

	IF ChiSq_try LT ChiSq THEN BREAK
	stepsize_try /= scale_down
    IF stepsize_try LT self.tolerance THEN RETURN, 1B ;Converged

  ENDWHILE

  ;forward in baby steps until increase in chisq

  Grad *= babystep

  WHILE 1 DO BEGIN

    stepsize = stepsize_try
    deformation_field = F_try
    deformed = D_try
    ChiSq = ChiSq_try

    stepsize_try += babystep
    F_try += Grad
    self->FFD, D_try, Source, F_try, xc, yc
	ChiSq_try = self->ChiSq(D_try, target)

    IF ChiSq_try GE ChiSq THEN RETURN, 0B
	IF stepsize_try GT stepsize_max THEN RETURN, 0B ;emergency stop

  ENDWHILE

END


;Source,Sx,Sy,DefField,Target,xc,yc, Weight_cnt, Weight_loc, Weight_val, DEF=Deformed

PRO MOCOMO_2D::GRADIENT, Gradient, Sx, Sy, residue, deformation_field, xc, yc, Weight_cnt, Weight_loc, Weight_val

;Deform the image gradient

  x = INTERPOLATE(deformation_field[*,0,*,*], xc, yc, /GRID)
  y = INTERPOLATE(deformation_field[*,1,*,*], xc, yc, /GRID)

  Dx = INTERPOLATE(Sx, long(x), y)
  Dy = INTERPOLATE(Sy, x, long(y))

;Multiply with residual

  Dx *= Residue
  Dy *= Residue

;Multiply with weighting factors

  Dx = Weight_val*Dx[Weight_loc]
  Dy = Weight_val*Dy[Weight_loc]

;Integrate over the mask

  ng = size(Gradient,/Dimensions)
  i0 = 0L
  FOR i=0L,ng[2]-1 DO BEGIN
    i1 = i0 + Weight_cnt[i] - 1
    Gradient[*,0,i] = TOTAL(Dx[i0:i1])
	Gradient[*,1,i] = TOTAL(Dy[i0:i1])
	i0 = i1 + 1
  ENDFOR

END


;Calculates weighting functions for each control point

;Inputs:
;	nD: two-element array, dimensions of grid of control points (Dx, Dy)
;	nW: two-element array, dimensions of the source image window (Wx, Wy)

;Outputs
; xc: X-coordinates of image pixel centres within window, Wx * Wy array, assuming distance between neighbouring control points = 1, (0,0) = lower left control point
; yc: Y-coordinates of image pixel centres within window, Wx * Wy array, assuming distance between neighbouring control points = 1, (0,0) = lower left control point

PRO MOCOMO_2D::WEIGHTS, nD, nw, xc, yc, Weight_cnt, Weight_loc, Weight_val

  ;determine distance between pixel centres, assuming distance between neighbouring control points = 1
  ;two-elements array
  ; ds[0] = distance in X-direction
  ; ds[1] = distance in Y-direction
  ;check that ds * (nW-1) = nD-1

  ndim = 2
  ds = (nD-1E)/(nW-1E)

  ;calculate X- and Y- coordinates of pixel centres

  xc = ds[0]*findgen(nW[0])
  yc = ds[1]*findgen(nW[1])

  ;calculate array rp of X- and Y-coordinates of pixel centres

  rp = fltarr([ndim,nw])
  FOR i=0L, nw[0]-1 DO BEGIN
  FOR j=0L, nw[1]-1 DO BEGIN
    rp[0,i,j] = xc[i] ;X-coordinate of pixel (i,j)
    rp[1,i,j] = yc[j] ;Y-coordinate of pixel (i,j)
  ENDFOR
  ENDFOR

  NumberOfControlPoints = product(nD) ;total number of control points
  NumberOfImagePixelsInWindow = product(nw) ;total number of image pixels in the window
  rp = REFORM(rp, ndim, NumberOfImagePixelsInWindow, /overwrite)

;Define the weighting functions for each control point
;Note each image pixel is part of 4 control point neighbourhoods

  Weight_cnt = lonarr(NumberOfControlPoints) ;size of the neighbourhood of each control point
  Weight_loc = lonarr((2^ndim)*NumberOfImagePixelsInWindow) ;indices of pixels in the neighbourhood of each control point
  Weight_val = fltarr((2^ndim)*NumberOfImagePixelsInWindow) ;values of weights in those pixels

;Define a mask on a control point neighbourhood with value=1 in the centre and 0 outside

;0 0 0
;0 1 0
;0 0 0

  Mask = fltarr(3,3)
  Mask[1,1] = 1

;Loop over control points
;	Move the mask over the control point
;	Move the image under the mask
;	Get the location of the pixels under the mask (Wi_loc)
;	Interpolate the mask on those pixels (Wi)

  i0 = 0L
  rb = array_indices(nD, lindgen(NumberOfControlPoints), /dimensions)
  FOR i=0L,NumberOfControlPoints-1 DO BEGIN

    xi = rp[0,*]-rb[0,i]
    yi = rp[1,*]-rb[1,i]

	; Wi_loc = locations of the pixels under the mask (index)
	; Wi_cnt = number of pixels under the mask
  	Wi_loc = where((abs(xi) lt 1) and (abs(yi) lt 1), Wi_cnt)

	; Interpolate the mask on those locations
  	Wi = INTERPOLATE(Mask, 1+xi[Wi_loc], 1+yi[Wi_loc])

	; Store all values for control point i in 1-dim arrays
    i1 = i0 + Wi_cnt - 1
	Weight_cnt[i] = Wi_cnt
	Weight_loc[i0:i1] = Wi_loc
	Weight_val[i0:i1] = Wi
	i0 = i1 + 1
  ENDFOR
END




FUNCTION MOCOMO_2D::FIT, deformed, deformation_field, Source, Target, xc, yc, Weight_cnt, Weight_loc, Weight_val

  stepsize = 5.0 ;initial stepsize in pixels

  ns = SIZE(Source, /Dimensions)
  Sx = Source[*,1:ns[1]-1,*] - Source[*,0:ns[1]-2,*] ;derivative of source image with respect to x
  Sy = Source[*,*,1:ns[2]-1] - Source[*,*,0:ns[2]-2] ;derivative of source image with respect to y

  nf = SIZE(deformation_field,/Dimensions)
  Gradient = FLTARR(nf[0], nf[1], Product(nf[2:*]), /NOZERO)

  FOR steps=0L, self.max_number_of_gradient_descent_steps DO BEGIN

    self->GRADIENT, Gradient, Sx, Sy, target-deformed, deformation_field, xc, yc, Weight_cnt, Weight_loc, Weight_val
    IF self->LINESEARCH(Gradient, Source, deformed, deformation_field, Target, xc, yc, STEPSIZE=stepsize) THEN BREAK
  ENDFOR

  RETURN, steps EQ 0
END




FUNCTION MOCOMO_2D::FIT_ALL, deformed, deformation_field, Source, Target, xc, yc, Weight_cnt, Weight_loc, Weight_val

   ns = SIZE(Source, /Dimensions)

   converged = 1B
   FOR t=0L,ns[0]-1 DO BEGIN

     d_t = deformed[t,*,*]
     f_t = deformation_field[t,*,*,*]

     converged *= self->FIT(d_t, f_t, Source[t,*,*], Target[t,*,*], xc, yc, Weight_cnt, Weight_loc, Weight_val)

     deformed[t,*,*] = d_t
     deformation_field[t,*,*,*] = f_t

  ENDFOR

  RETURN, converged
END



PRO MOCOMO_2D::OPTIMIZE, source, deformed, deformation_field

  nd = SIZE(deformed, /DIMENSIONS)
  nf = SIZE(deformation_field, /DIMENSIONS)

  self->WEIGHTS, nf[2:*], nd[1:*], xc, yc, Weight_cnt, Weight_loc, Weight_val

  FOR updates=1L, self.max_number_of_parameter_updates DO BEGIN

    self.model -> FIT, deformed, target
    no_change = self->FIT_ALL(deformed, deformation_field, source, target, xc, yc, Weight_cnt, Weight_loc, Weight_val)
    IF no_change THEN BREAK

  ENDFOR
END



PRO MOCOMO_2D::MULTIRESOLUTION, source, deformed, deformation_field

  self -> OPTIMIZE, source, deformed, deformation_field

  n_levels = N_ELEMENTS((*self.grid)[*,0])
  FOR resolution_level=1L, n_levels-1 DO BEGIN

    grid_size = reform((*self.grid)[resolution_level,*])
  	deformation_field = self->SCALEUP(deformation_field, grid_size)

    self -> OPTIMIZE, source, deformed, deformation_field

  ENDFOR
END



PRO MOCOMO_2D::APPLY, Source, deformation_field, PARAMETERS=Par, NO_MOCO=no_moco

  self -> SET_RESOLUTION_LEVELS
  ns = SIZE(Source, /DIMENSIONS)
  grid_size = reform((*self.grid)[0,*])
  deformation_field = self->NODEFORMATION(ns[0], grid_size)

  xw = self.win[0] + LINDGEN(self.win[2]) ;x-coordinates of window pixels
  yw = self.win[1] + LINDGEN(self.win[3]) ;y-coordinates of window pixels

  IF NOT KEYWORD_SET(no_moco) THEN BEGIN
  	deformed = Source[*, xw, yw]
  	self -> MULTIRESOLUTION, source, deformed, deformation_field
  	Source[*, xw, yw] = deformed
  ENDIF

  IF ARG_PRESENT(Par) THEN BEGIN
  	self.model -> PARAMETERS, Source[*, xw, yw], P
  	np = SIZE(P,/DIMENSIONS)
  	Par = FLTARR([np[0],ns[1:*]])
  	Par[*,xw,yw] = P
  ENDIF

END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		CLEANUP, INIT and DEFINE  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




PRO MOCOMO_2D::CLEANUP

  OBJ_DESTROY, self.model
  PTR_FREE, self.grid
END


FUNCTION MOCOMO_2D::INIT, Source, ModelName, Independent, $
  GRID_SIZE=resolution, TOLERANCE=tolerance, WINDOW=win

  ns = SIZE(Source, /Dimensions)

  IF N_ELEMENTS(resolution) EQ 0 THEN resolution = FLOOR(min(ns[1:*])/8E)
  IF N_ELEMENTS(tolerance) EQ 0 THEN tolerance = 1.0
  IF N_ELEMENTS(win) EQ 0 THEN win = {p:[0L,0L], n:ns[1:*]}

  self.resolution = resolution
  self.tolerance = tolerance
  self.win = [win.p[0],win.p[1],win.n[0],win.n[1]]
  self.model = OBJ_NEW('MoCoModel_' + ModelName, Independent)
  self.max_number_of_parameter_updates = 100
  self.max_number_of_gradient_descent_steps = 10000

  RETURN, 1
END


PRO MOCOMO_2D__DEFINE

  struct = {MOCOMO_2D $
    , resolution: 16L $
    , tolerance: 1E $
    , win: LONARR(4) $
    , model: obj_new() $
    , max_number_of_parameter_updates: 100L $
    , max_number_of_gradient_descent_steps: 10000L $
    , grid:ptr_new() $
    }


END