;Calculates weighting functions for each control point

;Inputs:
;	nD: two-element array, dimensions of grid of control points (Dx, Dy)
;	nW: two-element array, dimensions of the source image window (Wx, Wy)

;Outputs
; xc: X-coordinates of image pixel centres within window, Wx * Wy array, assuming distance between neighbouring control points = 1, (0,0) = lower left control point
; yc: Y-coordinates of image pixel centres within window, Wx * Wy array, assuming distance between neighbouring control points = 1, (0,0) = lower left control point

PRO FFD_2D_PRECOMPUTE, nD, nw, xc, yc, Weight_cnt, Weight_loc, Weight_val

  ;determine distance between pixel centres, assuming distance between neighbouring control points = 1
  ;two-elements array
  ; ds[0] = distance in X-direction
  ; ds[1] = distance in Y-direction
  ;check that ds * (nW-1) = nD-1

  ds = (nD-1E)/(nW-1E)

  ;calculate X- and Y- coordinates of pixel centres

  xc = ds[0]*findgen(nW[0])
  yc = ds[1]*findgen(nW[1])

  ;calculate array rp of X- and Y-coordinates of pixel centres

  rp = fltarr([2,nw])
  FOR i=0L, nw[0]-1 DO BEGIN
  FOR j=0L, nw[1]-1 DO BEGIN
;    rp[0,i,j] = ds[0]*i ;X-coordinate of pixel (i,j)
;    rp[1,i,j] = ds[1]*j ;Y-ccordinate of pixel (i,j)
    rp[0,i,j] = xc[i] ;X-coordinate of pixel (i,j)
    rp[1,i,j] = yc[j] ;Y-ccordinate of pixel (i,j)
  ENDFOR
  ENDFOR

  NumberOfControlPoints = product(nD) ;total number of control points
  NumberOfImagePixelsInWindow = product(nw) ;total number of image pixels in the window
  rp = REFORM(rp, 2, NumberOfImagePixelsInWindow, /overwrite)

;Define the weighting functions for each control point
;Note each image pixel is part of 4 control point neighbourhoods

  Weight_cnt = lonarr(NumberOfControlPoints) ;size of the neighbourhood of each control point
  Weight_loc = lonarr(4*NumberOfImagePixelsInWindow) ;indices of pixels in the neighbourhood of each control point
  Weight_val = fltarr(4*NumberOfImagePixelsInWindow) ;values of weights in those pixels

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