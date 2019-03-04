FUNCTION FFD_2D, S, D, xc, yc

  Di = INTERPOLATE(D, xc, yc, /GRID)

  x = REFORM(Di[0,*,*],/OVERWRITE)
  y = REFORM(Di[1,*,*],/OVERWRITE)

  return, INTERPOLATE(S, x, y)

END