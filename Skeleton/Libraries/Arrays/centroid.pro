FUNCTION Centroid, array

;    if data_chk(array,/ndim) NE 2 THEN BEGIN
;    Message, 'Array must be two-dimensional. Returning...', /Informational
;    RETURN, -1
;    endif

;    case 1 of
;        keyword_set(invert): marray=(max(array) - array)*(array ne 0)
;        else: marray=array
;    endcase
    marray=array
    totalMass = Total(marray)
    dimensions = size(marray,/dimensions)
    xcm = Total( Total(marray, 2) * lindgen(dimensions[0]) ) / totalMass
    ycm = Total( Total(marray, 1) * lindgen(dimensions[1]) ) / totalMass
    RETURN, [xcm, ycm]
END