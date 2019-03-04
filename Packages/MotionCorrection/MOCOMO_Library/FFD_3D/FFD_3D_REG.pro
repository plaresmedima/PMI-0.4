FUNCTION FFD_3D_REG, precision, Source, Target, xc, yc, zc, Weight_cnt, Weight_loc, Weight_val, DEFORMATION_FIELD = DefField, DEFORMED = Deformed

 itmax = 100. ;emergency stop
 stepsize = 5.0 ;pixels

 ns = size(Source, /Dimensions)

 Sx = Source[1:ns[0]-1,*,*] - Source[0:ns[0]-2,*,*]
 Sy = Source[*,1:ns[1]-1,*] - Source[*,0:ns[1]-2,*]
 Sz = Source[*,*,1:ns[2]-1] - Source[*,*,0:ns[2]-2]

 FOR iterations=1L, itmax DO BEGIN
   Grad = FFD_3D_GRAD(Source,Sx,Sy,Sz,DefField,Target,xc,yc, zc, Weight_cnt, Weight_loc, Weight_val, DEF=Deformed)
   IF FFD_3D_LSEARCH(Grad, Source, Target, xc, yc, zc, precision, DEFORMATION_FIELD=DefField, DEFORMED=Deformed, STEPSIZE=stepsize) THEN BREAK
 ENDFOR

 return, iterations EQ 1B

END