FUNCTION FFD_2D_INIT, p, nW, nD, nt

  db = (nW-1E)/(nD-1E)
  rb = fltarr([2,nD])

  FOR i=0L, nD[0]-1 DO BEGIN
  FOR j=0L, nD[1]-1 DO BEGIN
    rb[0,i,j] = p[0] + db[0]*i
    rb[1,i,j] = p[1] + db[1]*j
  ENDFOR
  ENDFOR

  D = fltarr([nt,2,nD])
  for t=0L,nt-1 do D[t,*,*,*] = rb

  return, D
END