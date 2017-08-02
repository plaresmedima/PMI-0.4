FUNCTION LINFIT_SVD, v, x, y, z, t

A= TRANSPOSE([[x],[y],[z],[t]])

SVDC, A, Wsvd, Usvd, Vsvd

Xlls = TRANSPOSE(Usvd) ## TRANSPOSE([v])
for K=0,n_elements(Wsvd)-1 DO IF Wsvd[K] GT 0 then Xlls[k] = Xlls[k]/Wsvd[k]

RETURN, Vsvd ## Xlls

END