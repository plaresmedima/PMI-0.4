;Definitions:
; (Sx, Sy) the dimensions (columns, rows) of the source image
; (Wx, Wy) the dimensions (columns, rows) of the window within the source image
; (Dx, Dy) the dimensions (columns, rows) of the grid of control points

;Inputs:
; S: Source Image, Sx x Sy array
; D: Deformation Field, 2 * Dx * Dy array, with D[0,*,*] = X-component and D[1,*,*] = Y-component
; xc: X-coordinates of image pixel centres within window, Wx * Wy array, assuming distance between neighbouring control points = 1
; yc: Y-coordinates of image pixel centres within window, Wx * Wy array, assuming distance between neighbouring control points = 1

FUNCTION FFD_2D, S, D, xc, yc

;Interpolate deformation field on all source image pixels
;Return value Di has dimension 2 * Wx * Wy

  Di = INTERPOLATE(D, xc, yc, /GRID)

;Extract x and y coordinates separately, remove first dimension
;Di_x and Di_y have dimension Wx * Wy

  Di_x = REFORM(Di[0,*,*],/OVERWRITE)
  Di_y = REFORM(Di[1,*,*],/OVERWRITE)

;Interpolate source image on new locations
;Return value has dimensions Wx * Wy

  return, INTERPOLATE(S, Di_x, Di_y)

END