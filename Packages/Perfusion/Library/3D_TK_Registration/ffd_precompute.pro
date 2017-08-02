PRO ffd_precompute, Source, nb

  COMMON ffd_const, pi, qi, ri, si, pi2, qi2, ri2, si2, p, q, r, s, p2, q2, r2, s2, xs, ys,zs, S_x, S_y, S_z, S_xy, S_yz, S_xz, S_xyz

  ns = size(Source, /dimensions)
  nx = ns[0]
  ny = ns[1]
  nz = ns[2]
  xind = findgen(nx)
  yind = findgen(ny)
  zind = findgen(nz)

  ds = (nb-1E)/ns ;(x,y)-distance between pixels (deformation field pixelsize=1)

  xc = ds[0]/2 + ds[0]*xind
  yc = ds[1]/2 + ds[1]*yind
  zc = ds[2]/2 + ds[2]*zind


  x = fltarr(nx,ny,nz)
  y = fltarr(nx,ny,nz)
  z = fltarr(nx,ny,nz)
  for i=0L,nx-1 do begin
  for j=0L,ny-1 do begin
  for k=0L,nz-1 do begin
    x[i,j,k] = xc[i]
    y[i,j,k] = yc[j]
    z[i,j,k] = zc[k]
  endfor
  endfor
  endfor


  i = long(x)
  j = long(y)
  k = long(z)
  u = x - i
  v = y - j
  w = z - k

  pi = i + j*nb[0] + k*nb[0]*nb[1]
  qi = i+1 + j*nb[0] + k*nb[0]*nb[1]
  ri = i + (j+1)*nb[0] + k*nb[0]*nb[1]
  si = i+1 + (j+1)*nb[0] + k*nb[0]*nb[1]

  pi2 = i + j*nb[0] + (k+1)*nb[0]*nb[1]
  qi2 = i+1 + j*nb[0] + (k+1)*nb[0]*nb[1]
  ri2 = i + (j+1)*nb[0] + (k+1)*nb[0]*nb[1]
  si2 = i+1 + (j+1)*nb[0] + (k+1)*nb[0]*nb[1]

  p = (1-u)*(1-v)*(1-w)
  q = u*(1-v)*(1-w)
  r = (1-u)*v*(1-w)
  s = u*v*(1-w)

  p2 = (1-u)*(1-v)*w
  q2 = u*(1-v)*w
  r2 = (1-u)*v*w
  s2 = u*v*w

  xs = fltarr(nx,ny,nz)
  ys = fltarr(nx,ny,nz)
  zs = fltarr(nx,ny,nz)
  for i=0L,nx-1 do begin
  for j=0L,ny-1 do begin
  for k=0L,nz-1 do begin
    xs[i,j,k] = xind[i]
    ys[i,j,k] = yind[j]
    zs[i,j,k] = zind[k]
  endfor
  endfor
  endfor

  S_x = Source[xind[1:nx-1],*,*] - Source[xind[0:nx-2],*,*]
  S_y = Source[*,yind[1:ny-1],*] - Source[*,yind[0:ny-2],*]
  S_z = Source[*,*,zind[1:nz-1]] - Source[*,*,zind[0:nz-2]]
  S_xy = S_x[*,yind[1:ny-1],*] - S_x[*,yind[0:ny-2],*]
  S_yz = S_y[*,*,zind[1:nz-1]] - S_y[*,*,zind[0:nz-2]]
  S_xz = S_x[*,*,zind[1:nz-1]] - S_x[*,*,zind[0:nz-2]]
  S_xyz = S_xy[*,*,zind[1:nz-1]] - S_xy[*,*,zind[0:nz-2]]

END