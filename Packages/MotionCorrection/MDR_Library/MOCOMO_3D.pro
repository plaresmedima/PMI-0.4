

;Main function performing 2D standard MoCoMo

;Calling sequence:
;	Deformed = MOCOMO_2D(Source, Model, Independent, Resolution, Precision, WIN=window, DEF_FIELD=deformation_field)

;Input:
;	Source: (nk,nx,ny)-element array with images
;	Model: string defining the model to be fitted
;	Independent: indepent parameters for the model (format defined by the model function)
;	Resolution: smallest spacing of deformation field control points (nr. of pixels)
;	Precision: stopping criterion for optimisation (units: pixel size).
;		Example: if precision = 0.5 then the optimisation will stop when the largest
;		improvement in the deformation field is less than 0.5 pixels.; 	wi
;	window (OPTIONAL) structure defining the window to be motion corrected
;		if this argument is not provided then the window defaults to the whole slice
;		window.p = coordinates of lower left corner
;		window.n = dimensions of window
;	deformation_field (OPTIONAL) named variable to hold the optimised deformation field

;Output:
;	Deformed: motion-corrected data


PRO MOCOMO_3D, Source, ModelName, Independent, $
  GRID_SIZE=resolution, TOLERANCE=tolerance, WINDOW=win, DEFORMATION_FIELD=deformation_field

  MoCoMo = OBJ_NEW('MOCOMO_3D', Source, ModelName, Independent, $
    GRID_SIZE=resolution, TOLERANCE=tolerance, WINDOW=win)

  MoCoMo -> APPLY, Source, deformation_field

  OBJ_DESTROY, MoCoMo

END