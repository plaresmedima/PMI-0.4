FUNCTION ffd, Source, B, Def_x, Def_y, Def_z

COMMON ffd_const, pi, qi, ri, si, pi2, qi2, ri2, si2, p, q, r, s, p2, q2, r2, s2, xs, ys, zs, S_x, S_y, S_z, S_xy, S_yz, S_xz, S_xyz

Bx = REFORM(B[0,*,*,*])
By = REFORM(B[1,*,*,*])
Bz = REFORM(B[2,*,*,*])

  x = xs + p*Bx[pi] + q*Bx[qi] + r*Bx[ri] + s*Bx[si] + p2*Bx[pi2] + q2*Bx[qi2] + r2*Bx[ri2] + s2*Bx[si2]
  y = ys + p*By[pi] + q*By[qi] + r*By[ri] + s*By[si] + p2*By[pi2] + q2*By[qi2] + r2*By[ri2] + s2*By[si2]
  z = zs + p*Bz[pi] + q*Bz[qi] + r*Bz[ri] + s*Bz[si] + p2*Bz[pi2] + q2*Bz[qi2] + r2*Bz[ri2] + s2*Bz[si2]

  ns = size(Source, /dimensions)
  Def = fltarr(ns)
  interior = where((x GE -0.5E) AND (x LE ns[0]-0.5E) AND (y GE -0.5E) AND (y LE ns[1]-0.5E) AND (z GE -0.5E) AND (z LE ns[2]-0.5E), cnt)
  if cnt eq 0 then begin
    if arg_present(Def_x) then Def_x = fltarr(ns)
    if arg_present(Def_y) then Def_y = fltarr(ns)
    if arg_present(Def_z) then Def_z = fltarr(ns)
    return, Def
  endif
  xi = x[interior]
  yi = y[interior]
  zi = z[interior]
  i = long(xi)
  j = long(yi)
  k = long(zi)
  u = xi - i
  v = yi - j
  w = zi - k
  Def[interior] = Source[i,j,k] + u*S_x[i,j,k] + v*S_y[i,j,k] + w*S_z[i,j,k] + u*v*S_xy[i,j,k] + v*w*S_yz[i,j,k]+ u*w*S_xz[i,j,k] + u*v*w*S_xyz[i,j,k]

  if arg_present(Def_x) then begin
    Def_x = fltarr(ns)
    Def_y = fltarr(ns)
    Def_z = fltarr(ns)
    Def_x[interior] = S_x[i,j,k] + u*S_xx[i,j,k] + v*S_xy[i,j,k] + w*S_xz[i,j,k] + u*v*S_xxy[i,j,k] + v*w*S_xyz[i,j,k]+ u*w*S_xxz[i,j,k] + u*v*w*S_xxyz[i,j,k]
    Def_y[interior] = S_y[i,j,k] + u*S_yx[i,j,k] + v*S_yy[i,j,k] + w*S_yz[i,j,k] + u*v*S_yxy[i,j,k] + v*w*S_yyz[i,j,k]+ u*w*S_yxz[i,j,k] + u*v*w*S_yxyz[i,j,k]
    Def_z[interior] = S_z[i,j,k] + u*S_zx[i,j,k] + v*S_zy[i,j,k] + w*S_zz[i,j,k] + u*v*S_zxy[i,j,k] + v*w*S_zyz[i,j,k]+ u*w*S_zxz[i,j,k] + u*v*w*S_zxyz[i,j,k]
  endif
  return, Def

END