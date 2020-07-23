FUNCTION MoCoModel_TwoCompartmentFiltration__UNITTEST_Data2, fixed, d, Parameters=P

  nt = d[0]
  nx = d[1]
  ny = d[2]

  x = FINDGEN(nx)/(nx-1E)
  y = FINDGEN(ny)/(ny-1E)

  v = (1+x*0) # (1+y)
  v = v/max(v)

  f = (2 - x) # (1+y)
  f = 0.9*f/max(f)

  vp = f*v
  ve = (1-f)*v

  Fp = (0.1 + x) # (1 + y*0)
  PS = 0.2 * (1 + x*0) # (0.1 + y)

  Tp = vp/Fp
  Te = ve/PS
  Tt = (vp+ve)/Fp

  S0 = 0.1 + (1 + 0*x) # (1 + 0*y)

  P = FLTARR(4,nx,ny)
  S = FLTARR(nt, nx, ny)

  P[0,*,*] = 1/(Tp*Te)
  P[1,*,*] = (Tp+Te)/(Tp*Te)
  P[2,*,*] = Fp
  P[3,*,*] = Fp*Tt/(Tp*Te)

  FOR i=0L, nx-1 DO BEGIN
  FOR j=0L, ny-1 DO BEGIN
    Tpos = max([Te[i,j],Tp[i,j]])
    Tneg = min([Te[i,j],Tp[i,j]])
    Epos = ExpConvolution(1/Tpos,[fixed.t,fixed.ca])
    Eneg = ExpConvolution(1/Tneg,[fixed.t,fixed.ca])
    Apos = (Tt[i,j]-Tneg)/(Tpos-Tneg)
    Aneg = (Tpos-Tt[i,j])/(Tpos-Tneg)
    S[*,i,j] = S0[i,j] + Fp[i,j] * (Apos*Epos + Aneg*Eneg)
  ENDFOR
  ENDFOR

  RETURN, S
END

FUNCTION MoCoModel_TwoCompartmentFiltration__UNITTEST_Data, fixed, d, Parameters=P

  nt = d[0]
  nx = d[1]
  ny = d[2]

  x = FINDGEN(nx)/(nx-1E)
  y = FINDGEN(ny)/(ny-1E)

  v = 0.1 + exp(-((x-0.5)/1.0)^2) # exp(-((y-0.5)/1.0)^2)
  v = v/max(v)

  f = (1 + x) # (1+sqrt(y))
  f = 0.9*f/max(f)

  vp = f*v
  ve = (1-f)*v

  Fp = (0.1 + x) # (1 + y*0)
  PS = 0.2 * (1 + x*0) # (0.1 + y)

  Tp = vp/Fp
  Te = ve/PS
  Tt = (vp+ve)/Fp

  S0 = 0.1 + (1 + 0*x) # (1 + 0*y)

  P = FLTARR(4,nx,ny)
  S = FLTARR(nt, nx, ny)

  P[0,*,*] = 1/(Tp*Te)
  P[1,*,*] = (Tp+Te)/(Tp*Te)
  P[2,*,*] = Fp
  P[3,*,*] = Fp*Tt/(Tp*Te)

  FOR i=0L, nx-1 DO BEGIN
  FOR j=0L, ny-1 DO BEGIN
    Tpos = max([Te[i,j],Tp[i,j]])
    Tneg = min([Te[i,j],Tp[i,j]])
    Epos = ExpConvolution(1/Tpos,[fixed.t,fixed.ca])
    Eneg = ExpConvolution(1/Tneg,[fixed.t,fixed.ca])
    Apos = (Tt[i,j]-Tneg)/(Tpos-Tneg)
    Aneg = (Tpos-Tt[i,j])/(Tpos-Tneg)
    S[*,i,j] = S0[i,j] + Fp[i,j] * (Apos*Epos + Aneg*Eneg)
  ENDFOR
  ENDFOR

  RETURN, S
END



PRO MoCoModel_TwoCompartmentFiltration__UNITTEST

  nx = 10
  ny = 128
  nt = 120
  NSR = 0.0

  dt = 0.1 ;sec
  t = dt*findgen(nt)
  ca = t^2*exp(-0.4*t)

  fixed_parameters = {t:t, ca:ca, n0:10}

  S = MoCoModel_TwoCompartmentFiltration__UNITTEST_Data2(fixed_parameters, [nt,nx,ny], Parameters=P)
  Snoise = S + NSR*max(S)*RANDOMU(X,[nt, nx, ny])

  Model = OBJ_NEW('MoCoModel_TwoCompartmentFiltration', fixed_parameters)

  time = systime(1)
  FOR i=1L, 1 DO BEGIN
    Model -> PARAMETERS, Snoise, Prec
    Model -> FIT, Snoise, Srec
    Model -> RESIDUE, Snoise, Prec, Sres
    Sder = Model -> GROUP_RESIDUE(Prec, Snoise)
  ENDFOR
  print, 'Calculation time (sec): ', systime(1)-time

  OBJ_DESTROY, Model


;;;;;;;;;;;;;;;;;;;
;;DISPLAY RESULTS;;
;;;;;;;;;;;;;;;;;;;


  DEVICE, Decomposed = 0
  DEVICE, Bypass_Translation=0

  WINDOW, 1, xsize=nt*nx, ysize=5*ny, xpos=0, ypos=ny

  WSET, 1

  row = 0
  FOR i=0L,3 DO BEGIN
    Smin = min(Prec[i,*,*], max=Smax)
    S = (!D.TABLE_SIZE-1) * (P[i,*,*] - Smin)/(Smax - Smin)
    TV, REFORM(S), i*2
    S = (!D.TABLE_SIZE-1) * (Prec[i,*,*] - Smin)/(Smax - Smin)
    TV, REFORM(S), i*2+1
  ENDFOR

  row += 1
  S = Snoise
  S = (S - min(S))/(max(S) - min(S)) * (!D.TABLE_SIZE-1)
  FOR i=0L, nt-1 DO TV, REFORM(S[i,*,*]), row*nt+i

  row +=1
  S = Srec
  S = (S - min(S))/(max(S) - min(S)) * (!D.TABLE_SIZE-1)
  FOR i=0L, nt-1 DO TV, REFORM(S[i,*,*]), row*nt+i

  row +=1
  S = Sres
  S = (S - min(S))/(max(S) - min(S)) * (!D.TABLE_SIZE-1)
  FOR i=0L, nt-1 DO TV, REFORM(S[i,*,*]), row*nt+i

  row +=1
  S = Sder
  S = (S - min(S))/(max(S) - min(S)) * (!D.TABLE_SIZE-1)
  FOR i=0L, nt-1 DO TV, REFORM(S[i,*,*]), row*nt+i

END