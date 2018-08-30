;
;
;    Copyright (C) 2018 Steven Sourbron
;

;

PRO MOCOMO_2D_DCE_2CM__DEFINE

  struct = {MOCOMO_2D_DCE_2CM, $
   	INHERITS MOCOMO_2D, $
   	INHERITS MOCOMO_DCE_2CM }

END

;Source: 3D dataset with source image (2D+time)
;resolution: in pixel sizes
;precision: in pixel sizes
;time: array of time points
;aif: array of ca values
;nbaseline: nr of baseline values

;returns: coregistered source image

FUNCTION MOCOMO_2D_DCE_2CM, Source, resolution, precision, time, aif, nbaseline, Win=Win

	Source = TRANSPOSE(Source, [2,0,1])
    MOCOMO = OBJ_NEW('MOCOMO_2D_DCE_2CM', $
    	ptr_new(Source), resolution, precision, [Time, aif, nbaseline], Win=Win)
    Deformed = TRANSPOSE(MOCOMO->deformed(), [1,2,0])
    OBJ_DESTROY, MOCOMO

	return, Deformed

END


