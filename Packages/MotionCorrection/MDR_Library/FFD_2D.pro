FUNCTION FFD_2D, S, D

  nS = SIZE(S, /DIMENSIONS)
  nD = SIZE(D, /DIMENSIONS)
  ds = (nD[1:*]-1E)/(nS-1E)

  xc = ds[0]*findgen(nS[0])
  yc = ds[1]*findgen(nS[1])

  Dx = INTERPOLATE(D[0,*,*], xc, yc, /GRID)
  Dy = INTERPOLATE(D[1,*,*], xc, yc, /GRID)

  RETURN, INTERPOLATE(S, Dx, Dy)

END