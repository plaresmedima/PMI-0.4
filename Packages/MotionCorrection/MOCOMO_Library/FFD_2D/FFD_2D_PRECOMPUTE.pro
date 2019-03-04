PRO FFD_2D_PRECOMPUTE, nb, nw, xc, yc, Weight_cnt, Weight_loc, Weight_val

  ds = (nb-1E)/(nw-1E)

  xc = ds[0]*findgen(nw[0])
  yc = ds[1]*findgen(nw[1])

  rp = fltarr([2,nw])
  FOR i=0L, nw[0]-1 DO BEGIN
  FOR j=0L, nw[1]-1 DO BEGIN
    rp[0,i,j] = ds[0]*i
    rp[1,i,j] = ds[1]*j
  ENDFOR
  ENDFOR
  rp = REFORM(rp, 2, product(nw), /overwrite)

  n_b = product(nb)
  n_s = product(nw)

  Weight_cnt=lonarr(n_b)
  Weight_loc=lonarr(4*n_s)
  Weight_val=fltarr(4*n_s)

  Func = fltarr(3,3)
  Func[1,1] = 1

  i0 = 0L
  rb = array_indices(nb, lindgen(n_b), /dimensions)
  FOR i=0L,n_b-1 DO BEGIN

    ui = rp[0,*]-rb[0,i]
    vi = rp[1,*]-rb[1,i]

  	Wi_loc = where((abs(ui) lt 1) and (abs(vi) lt 1), Wi_cnt)
  	Wi = INTERPOLATE(Func, 1+ui[Wi_loc], 1+vi[Wi_loc])

    i1 = i0 + Wi_cnt - 1
	Weight_cnt[i] = Wi_cnt
	Weight_loc[i0:i1] = Wi_loc
	Weight_val[i0:i1] = Wi
	i0 = i1 + 1
  ENDFOR
END