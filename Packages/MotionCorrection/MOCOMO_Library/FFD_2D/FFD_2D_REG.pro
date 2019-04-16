;Inputs:
;	Precision: scalar value in pixel units specifying stopping criterion
;	Source: Source image (Nx * Ny)
;	Target: target image (Nx * Ny)
;	DefField: initial value for the deformation field
;	OUTPUTS of the FFD_2d_PRECOMPUTE procedure:
; 		xc
; 		yc
;		Weight_cnt
;		Weight_loc
;		Weight_val


;Outputs
;	DefField: optimal deformation field
;	Deformed: Deformed Source Image

FUNCTION FFD_2D_REG, precision, Source, Target, xc, yc, Weight_cnt, Weight_loc, Weight_val, DEFORMATION_FIELD = DefField, DEFORMED = Deformed

 itmax = 100. ;emergency stop
 stepsize = 5.0 ;initial stepsize in pixels

 ;Precompute gradient of the source image
 ns = size(Source, /Dimensions) ;x- and y- dimensions of the source image
 Sx = Source[1:ns[0]-1,*] - Source[0:ns[0]-2,*] ;derivative of source image with respect to x
 Sy = Source[*,1:ns[1]-1] - Source[*,0:ns[1]-2] ;derivative of source image with respect to y

 FOR iterations=1L, itmax DO BEGIN
   Grad = FFD_2D_GRAD(Source,Sx,Sy,DefField,Target,xc,yc, Weight_cnt, Weight_loc, Weight_val, DEF=Deformed)
   IF FFD_2D_LSEARCH(Grad, Source, Target, xc, yc, precision, DEFORMATION_FIELD=DefField, DEFORMED=Deformed, STEPSIZE=stepsize) THEN BREAK
 ENDFOR

 return, iterations EQ 1B

END