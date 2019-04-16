PRO MOCOMO_2D_OPTIMISE, Source, Deformed, DefField, Model, Independent, Precision, ns, nw, nb

  iter = 100 ;emergency stop

  FFD_2D_PRECOMPUTE, nb, nw, xc, yc, Weight_cnt, Weight_loc, Weight_val

  FOR it=1L,iter DO BEGIN

    FOR i=0L,Product(nw)-1 DO BEGIN
      r = array_indices(nw, i, /dimensions)
      S = REFORM(Deformed[*,r[0],r[1]])
      S = CALL_FUNCTION(Model+'_FIT', S, independent)
      Deformed[*,r[0],r[1]] = S
    ENDFOR

    converged = 1B
    FOR t=0L,ns[0]-1 DO BEGIN

      DefField_t = REFORM(DefField[t,*,*,*],/overwrite)

      converged *= FFD_2D_REG(precision, $
 	    REFORM(Source[t,*,*],/overwrite), $
 	    REFORM(Deformed[t,*,*],/overwrite), $
 	    xc, yc, Weight_cnt, Weight_loc, Weight_val, $
 	    DEFORMATION_FIELD = DefField_t, DEFORMED = Deformed_t)

      Deformed[t,*,*] = Deformed_t
      DefField[t,*,*,*] = DefField_t

    ENDFOR
    IF converged THEN BREAK

  ENDFOR

END

FUNCTION MOCOMO_2D_SCALEUP, DefField, nb

  nold = size(DefField, /dimensions)
  dr = (nold[2:*]-1E)/(nb-1E)

  X = dr[0] * findgen(nb[0])
  Y = dr[1] * findgen(nb[1])

  RETURN, INTERPOLATE(DefField, X, Y, /GRID)

END


FUNCTION MOCOMO_2D_INIT, nt, nw, nb, p0

  db = (nw-1E)/(nb-1E)
  rb = fltarr([2,nb])

  FOR i=0L, nb[0]-1 DO BEGIN
  FOR j=0L, nb[1]-1 DO BEGIN
    rb[0,i,j] = p0[0] + db[0]*i
    rb[1,i,j] = p0[1] + db[1]*j
  ENDFOR
  ENDFOR

  P = fltarr([nt,2,nb])
  for t=0L,nt-1 do P[t,*,*,*] = rb

  return, P

END

FUNCTION MOCOMO_2D_GRID, nw, Resolution

  nCells = ceil(float(max(nw))/Resolution)
  Order = 0L
  While 2L^Order LT nCells DO Order += 1
  nCells = 2L^lindgen(Order)
  FOVnorm = float(nw)/max(nw)
  nb = lonarr(Order,2)
  for i=0L, Order-1 do nb[i,*] = 1 + ceil(FOVnorm*nCells[i])
  RETURN, nb

END

FUNCTION MOCOMO_2D, Source, Model, Independent, Resolution, Precision, WIN=Win

  ns = size(Source, /Dimensions)
  nw = ns[1:*]
  p0 = lonarr(2)

  if n_elements(Win) ne 0 then begin
  	nw = Win.n
  	p0 = Win.p
  endif

  nDef = MOCOMO_2D_GRID(nw, Resolution)

  Independent = CALL_FUNCTION(Model+'_PRECOMPUTE', Independent)

  xw = p0[0] + lindgen(nw[0])
  yw = p0[1] + lindgen(nw[1])

  Deformed = Source[*, xw, yw]

  DefField = MOCOMO_2D_INIT(ns[0], nw, reform(nDef[0,*]), p0)
  MOCOMO_2D_OPTIMISE, Source, Deformed, DefField, Model, Independent, Precision, ns, nw, reform(nDef[0,*])

  FOR i=1L,n_elements(nDef[*,0])-1 DO BEGIN
  	  DefField = MOCOMO_2D_SCALEUP(DefField, reform(nDef[i,*]))
  	  MOCOMO_2D_OPTIMISE, Source, Deformed, DefField, Model, Independent, Precision, ns, nw, reform(nDef[i,*])
  ENDFOR

  Source[*, xw, yw] = Deformed

  return, Source
END