function PatlakLinMatrix, time, aif

  nt = n_elements(time)

  Matrix = fltarr(2,nt)

  Matrix[0,*] = Aif
  Matrix[1,*] = IntVector(Time,Aif)

  svdc, Matrix, Wmat, Umat, Vmat

  if Wmat[0] gt 0 then Umat[0,*] = Umat[0,*]/Wmat[0] else Umat[0,*] = Umat[0,*]*0
  if Wmat[1] gt 0 then Umat[1,*] = Umat[1,*]/Wmat[1] else Umat[1,*] = Umat[1,*]*0

  MatrixInv = Vmat ## transpose(Umat)

  return, MatrixInv

end