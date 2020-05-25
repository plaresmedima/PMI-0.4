;+
;NAME:
;  DOWNSAMPLE
;PURPOSE:
;  Downsample 1D, 2D, or 3D array, summing the original array over the
;  domain of the new pixels. Works like REBIN, except that the new size
;  does not have to be an integer multiple of the original size. For most
;  purposes, this procedure is preferable to interpolation when downsampling.
;  This routine will also upsample if desired, in which case the action is
;  equivalent to using CONGRID with cubic convolutional interpolation.
;CALLING SEQUENCE:
;  result = downsample(array, Nx [, Ny [, Nz]])
;RETURN VALUE:
;  The result is an array downsampled to the requested dimensions.
;ARGUMENTS:
;  Nx = Size of first dimension of the result.
;  Ny = Size of the second dimension of the result (required if array is 2-3D,
;     ignored otherwise).
;  Nz = Size of the third dimension of the result (required if array is 3D,
;     ignored otherwise).
;ALGORITHM:
;  First, the array is upsampled to a size that is an integer multiple of
;  (Nx, Ny, Nz) by CONGRID using cubic convolutional interpolation. Then
;  the image is decimated to the desired size using REBIN. Note that for 3D
;  arrays, CONGRID should revert to linear interpolation (I haven't actually
;  tested this).
;MODIFICATION HISTORY:
;  2009-Aug-11  C. Kankelborg
function downsample, array, Nx, Ny, Nz
;-

asize = size(array)
case asize[0] of
   1: begin
      Nx0 = asize[1]
      ratio = ceil(float(Nx0)/float(Nx))
      Nx1 = ratio * Nx
      array1 = congrid(array, Nx1, cubic=-0.5, /center)
      result = rebin(array1, Nx)
   end
   2: begin
      Nx0 = asize[1]
      ratio = ceil(float(Nx0)/float(Nx))
      Nx1 = ratio * Nx
      Ny0 = asize[2]
      ratio = ceil(float(Ny0)/float(Ny))
      Ny1 = ratio * Ny
      array1 = congrid(array, Nx1, Ny1, cubic=-0.5, /center)
      result = rebin(array1, Nx, Ny)
   end
   3: begin
      Nx0 = asize[1]
      ratio = ceil(float(Nx0)/float(Nx))
      Nx1 = ratio * Nx
      Ny0 = asize[2]
      ratio = ceil(float(Ny0)/float(Ny))
      Ny1 = ratio * Ny
      Nz0 = asize[3]
      ratio = ceil(float(Nz0)/float(Nz))
      Nz1 = ratio * Nz
      array1 = congrid(array, Nx1, Ny1, Nz1, cubic=-0.5, /center)
      result = rebin(array1, Nx, Ny, Nz)
   end
   else: message,'Input array must be 1D, 2D, or 3D.'
endcase

return, result
end