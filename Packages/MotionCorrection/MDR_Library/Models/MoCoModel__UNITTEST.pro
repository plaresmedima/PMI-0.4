PRO MoCoModel__UNITTEST

  Model = OBJ_NEW('MoCoModel', 1)

  nx = 100
  ny = 128
  nt = 15
  NSR = 2.0

  S0 = FINDGEN(1, nx, ny)^2
  Si = FLTARR(nt, nx, ny)

  Model -> FORWARD, Si, S0, S
  Snoise = S + NSR*max(S)*RANDOMU(X,[nt, nx, ny])

  Model -> PARAMETERS, Snoise, S0rec
  Model -> FIT, Snoise, Srec
  Model -> RESIDUE, Snoise, S0rec, Sres
  Sder = Model -> RESIDUE_PDER(S0rec, Snoise, 8)

  OBJ_DESTROY, Model


;;;;;;;;;;;;;;;;;;;
;;DISPLAY RESULTS;;
;;;;;;;;;;;;;;;;;;;


  DEVICE, Decomposed = 0
  DEVICE, Bypass_Translation=0

  WINDOW, 1, xsize=nt*nx, ysize=5*ny, xpos=0, ypos=ny

  WSET, 1

  row = 0
  S = S0
  S = (S - min(S0))/(max(S0) - min(S0)) * (!D.TABLE_SIZE-1)
  TV, REFORM(S), 0
  S = S0rec
  S = (S - min(S0rec))/(max(S0rec) - min(S0rec)) * (!D.TABLE_SIZE-1)
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