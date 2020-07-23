PRO DTI_Parameters, Par, M

 d = size(Par,/dimensions)
 M = FLTARR(d[0],d[1],5)

 ;0: Apparent Diffusion Coefficient
 ;1: Fractional Anisotropy
 ;2: Linear Anisotropy
 ;3: Planar Anisotropy
 ;4: Spherical Anisotropy

 Tensor = FLTARR(3,3)

 FOR x=0L, d[0]-1 DO BEGIN
   FOR y=0L, d[1]-1 DO BEGIN

     Tensor[0,0] = Par[x,y,0]
     Tensor[1,1] = Par[x,y,1]
     Tensor[2,2] = Par[x,y,2]
     Tensor[0,1] = Par[x,y,3]
     Tensor[1,0] = Par[x,y,3]
     Tensor[1,2] = Par[x,y,4]
     Tensor[2,1] = Par[x,y,4]
     Tensor[2,0] = Par[x,y,5]
     Tensor[0,2] = Par[x,y,5]

     TRIRED, Tensor, V, E
     TRIQL, V, E, Tensor ;Eigenvectors in the rows

     V = V[REVERSE(SORT(V))]
     Trace = TOTAL(V)

     M[x,y,0] = Trace/3
     M[x,y,1] = SQRT(3*TOTAL((V-M[x,y,0])^2)) / SQRT(2*TOTAL(V^2))
	 M[x,y,2] = (V[0]-V[1])/Trace
	 M[x,y,3] = (V[1]-V[2])*2/Trace
	 M[x,y,4] = V[2]*3/Trace

   ENDFOR
 ENDFOR
END