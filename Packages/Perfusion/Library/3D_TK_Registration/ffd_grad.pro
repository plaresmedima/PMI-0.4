FUNCTION ffd_grad, Source, Target, B

  Bx = REFORM(B[0,*,*,*])
  By = REFORM(B[1,*,*,*])
  Bz = REFORM(B[2,*,*,*])

  n = size(Bx, /dimensions)

  n = n[0]*n[1]*n[2]
  if n_elements(Gx) eq 0 then Gx = fltarr(n)
  if n_elements(Gy) eq 0 then Gy = fltarr(n)
  if n_elements(Gz) eq 0 then Gz = fltarr(n)
  Res = Target - ffd(Source, B, S_xD, S_yD, S_zD)

  Rx = Res * S_xD
  Ry = Res * S_yD
  Rz = Res * S_zD

  COMMON ffd_grad_const, Weight, Weight_cnt, Weight_loc

  i0=0L
  FOR i=0L,n-1 DO BEGIN
    Wi_cnt = Weight_cnt[i]
    Wi_loc = Weight_loc[i0:i0+Wi_cnt-1]
    Wi = Weight[i0:i0+Wi_cnt-1]
    i0 = i0+Wi_cnt
    Gx[i] = - total(Wi*Rx[Wi_loc])
	Gy[i] = - total(Wi*Ry[Wi_loc])
	Gz[i] = - total(Wi*Rz[Wi_loc])
  ENDFOR
  Gnorm = max(sqrt(Gx^2+Gy^2+Gz^2))
  Gx = Gx/Gnorm
  Gy = Gy/Gnorm
  Gz = Gz/Gnorm

  G = B
  G[0,*,*,*] = Gx
  G[1,*,*,*] = Gy
  G[2,*,*,*] = Gz

 RETURN,G

END