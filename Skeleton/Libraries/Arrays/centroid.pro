FUNCTION Centroid, array

    marray = float(array)
    totalMass = Total(marray)
    dimensions = size(marray,/dimensions)

    xcm = Total( Total(marray, 2) * lindgen(dimensions[0]) ) / totalMass
    ycm = Total( Total(marray, 1) * lindgen(dimensions[1]) ) / totalMass

    RETURN, [xcm, ycm]

END