FUNCTION FFD_3D, S, D, xc, yc, zc

  Di = INTERPOLATE(D, xc, yc, zc, /GRID)

  x = REFORM(Di[0,*,*,*],/OVERWRITE)
  y = REFORM(Di[1,*,*,*],/OVERWRITE)
  z = REFORM(Di[2,*,*,*],/OVERWRITE)

  return, INTERPOLATE(S, x, y, z)

END