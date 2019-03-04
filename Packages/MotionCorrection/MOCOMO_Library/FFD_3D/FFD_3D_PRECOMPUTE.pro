PRO FFD_3D_PRECOMPUTE, nb, nw, xc, yc, zc, Weight_cnt, Weight_loc, Weight_val

  ds = (nb-1E)/(nw-1E)

  xc = ds[0]*findgen(nw[0])
  yc = ds[1]*findgen(nw[1])
  zc = ds[2]*findgen(nw[2])

  rp = fltarr([3,nw])
  FOR i=0L, nw[0]-1 DO BEGIN
  FOR j=0L, nw[1]-1 DO BEGIN
  FOR k=0L, nw[2]-1 DO BEGIN
    rp[0,i,j,k] = ds[0]*i
    rp[1,i,j,k] = ds[1]*j
    rp[2,i,j,k] = ds[2]*k
  ENDFOR
  ENDFOR
  ENDFOR
  rp = REFORM(rp, 3, product(nw), /overwrite)

  n_b = product(nb)
  n_s = product(nw)

  Weight_cnt=lonarr(n_b)
  Weight_loc=lonarr(8*n_s)
  Weight_val=fltarr(8*n_s)

  Func = fltarr(3,3,3)
  Func[1,1,1] = 1

  i0 = 0L
  rb = array_indices(nb, lindgen(n_b), /dimensions)
  FOR i=0L,n_b-1 DO BEGIN

    ui = rp[0,*]-rb[0,i]
    vi = rp[1,*]-rb[1,i]
    wi = rp[2,*]-rb[2,i]

  	Wi_loc = where((abs(ui) lt 1) and (abs(vi) lt 1) and (abs(wi) lt 1), Wi_cnt)
  	Wi = INTERPOLATE(Func, 1+ui[Wi_loc], 1+vi[Wi_loc], 1+wi[Wi_loc])

    i1 = i0 + Wi_cnt - 1
	Weight_cnt[i] = Wi_cnt
	Weight_loc[i0:i1] = Wi_loc
	Weight_val[i0:i1] = Wi
	i0 = i1 + 1
  ENDFOR
END