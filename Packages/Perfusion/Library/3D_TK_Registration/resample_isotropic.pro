FUNCTION resample_isotropic, Source, voxel_sizes,matrix_new, voxel_size_new=voxel_size_new

	n = size(Source, /dimensions)

    FOV_x = n[0]*voxel_sizes[0]
    slab_thickness = n[2]*voxel_sizes[2]
    voxel_size_new = FOV_x/matrix_new[0]

    n0_new = matrix_new[0]
    n1_new = matrix_new[1]
    n2_new = round(slab_thickness/voxel_size_new)

    X = (n[0]-1)*findgen(n0_new)/(n0_new-1E)
    Y = (n[1]-1)*findgen(n1_new)/(n1_new-1E)
    Z = (n[2]-1)*findgen(n2_new)/(n2_new-1E)

	Source_iso = fltarr(n0_new, n1_new, n2_new, n[3])

	FOR t=0L,n[3]-1 DO BEGIN
		Source_iso[*,*,*,t] = INTERPOLATE(REFORM(Source[*,*,*,t]), X,Y,Z, /GRID)
	ENDFOR

	return, Source_iso

END