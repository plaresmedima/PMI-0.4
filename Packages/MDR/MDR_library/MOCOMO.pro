


PRO MOCOMO, Source, ModelName, Independent, $
  GRID_SIZE=grid_size, TOLERANCE=tolerance, WINDOW=win, $
  DEFORMATION_FIELD=deformation_field

  CASE SIZE(Source,/N_DIMENSIONS) OF
  	3: Class = 'MOCOMO_2D'
  	4: Class = 'MOCOMO_3D'
  ENDCASE

  Obj = OBJ_NEW(Class, Source, ModelName, Independent, $
    GRID_SIZE=grid_size, TOLERANCE=tolerance, WINDOW=win)

  Obj -> APPLY, Source, deformation_field

  OBJ_DESTROY, Obj

END