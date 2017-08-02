function pkreg_mres, Time, Ca, Source, Baseline

  Nt = n_elements(Source[0,0,0,*])
  nB = [2L,4L,8L,16L]
  itmax = [4.,4.,4.,4.]
  it = 0L
  B = fltarr(3,nB[it],nB[it],nB[it],Nt)

  pkreg_pro, Time, Ca, Source, Baseline, B=B,itmax=4., step=0.1

  FOR it=1L,n_elements(nB)-1 DO BEGIN
      B = ffd_interpol(B, [nB[it],nB[it],nB[it]])
      pkreg_pro, Time, Ca, Source, Baseline, B=B, itmax=itmax[it], step=0.1
  ENDFOR

  reg=source

 FOR t=0l,Nt-1 DO BEGIN
     ffd_precompute, REFORM(Source[*,*,*,t]), size(REFORM(B[0,*,*,*,0]), /dimensions)
  	 reg[*,*,*,t] = ffd(REFORM(Source[*,*,*,t]), REFORM(B[*,*,*,*,t]))
  ENDFOR
  return, reg

end
