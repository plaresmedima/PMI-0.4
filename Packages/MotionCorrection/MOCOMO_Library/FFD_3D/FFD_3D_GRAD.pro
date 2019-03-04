FUNCTION FFD_3D_GRAD,S,Sx,Sy,Sz,F,T,xc,yc,zc, W_cnt, W_loc, W_val,DEF=Def

  Di = INTERPOLATE(F, xc, yc, zc, /GRID)

  x = REFORM(Di[0,*,*,*],/OVERWRITE)
  y = REFORM(Di[1,*,*,*],/OVERWRITE)
  z = REFORM(Di[2,*,*,*],/OVERWRITE)

  Def = INTERPOLATE(S, x, y, z)

  Res = T - Def

  Dx = Res*INTERPOLATE(Sx, long(x), y, z)
  Dy = Res*INTERPOLATE(Sy, x, long(y), z)
  Dz = Res*INTERPOLATE(Sz, x, y, long(z))

  Dx = W_val*Dx[W_loc]
  Dy = W_val*Dy[W_loc]
  Dz = W_val*Dz[W_loc]

  nF = size(F,/Dimensions)
  n = Product(nF[1:*])
  Gradient = FLTARR(3, n, /nozero)

  i0=0L
  FOR i=0L,n-1 DO BEGIN
    i1 = i0 + (W_cnt)[i] - 1
    Gradient[0,i] = TOTAL(Dx[i0:i1])
	Gradient[1,i] = TOTAL(Dy[i0:i1])
	Gradient[2,i] = TOTAL(Dz[i0:i1])
	i0 = i1 + 1
  ENDFOR

  return, REFORM(Gradient, nF, /OVERWRITE)

END