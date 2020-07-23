PRO MoCoModel_Constant__UNITTEST

  Model = OBJ_NEW('MoCoModel_Constant', 1)

  nx = 100
  ny = 128
  nt = 15
  NSR = 1.0

  P = FINDGEN(1,nx,ny)^2
  S = FLTARR(nt, nx, ny)

  S = Model -> FORWARD(P,S)
  Snoise = S + NSR*max(S)*RANDOMU(X,[nt, nx, ny])

  time = systime(1)
  FOR i=1L, 1000 DO BEGIN
    Prec = Model -> PARAMETERS(Snoise)
    Srec = Model -> FIT(Snoise)
    Sres = Model -> RESIDUE(Prec,Snoise)
    Sder = Model -> RESIDUE_PDER(Prec, Snoise, 8)
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
  S = P
  S = (S - min(P))/(max(P) - min(P)) * (!D.TABLE_SIZE-1)
  TV, REFORM(S), 0
  S = Prec
  S = (S - min(Prec))/(max(Prec) - min(Prec)) * (!D.TABLE_SIZE-1)
  TV, REFORM(S), 1

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