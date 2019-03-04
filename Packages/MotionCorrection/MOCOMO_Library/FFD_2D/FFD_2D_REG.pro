FUNCTION FFD_2D_REG, precision, Source, Target, xc, yc, Weight_cnt, Weight_loc, Weight_val, DEFORMATION_FIELD = DefField, DEFORMED = Deformed

 itmax = 100. ;emergency stop
 stepsize = 5.0 ;pixels

 ns = size(Source, /Dimensions)

 Sx = Source[1:ns[0]-1,*] - Source[0:ns[0]-2,*]
 Sy = Source[*,1:ns[1]-1] - Source[*,0:ns[1]-2]

 FOR iterations=1L, itmax DO BEGIN
   Grad = FFD_2D_GRAD(Source,Sx,Sy,DefField,Target,xc,yc, Weight_cnt, Weight_loc, Weight_val, DEF=Deformed)
   IF FFD_2D_LSEARCH(Grad, Source, Target, xc, yc, precision, DEFORMATION_FIELD=DefField, DEFORMED=Deformed, STEPSIZE=stepsize) THEN BREAK
 ENDFOR

 return, iterations EQ 1B

END