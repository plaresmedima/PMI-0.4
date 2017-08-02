;Description
;-----------
;
;Interpolates the current deformation field to have the same
;dimensions as measured source data using trilinear interpolation
;
;
;Syntax
;------
;
;Deformed = ffd_interpol(B, nint)
;
;
;Arguments
;---------
;
;B: Array of deformation field which interpolation is desired
;nint: integer number specifies the dimensions needed
;
;
;Returns
;-------
;
;Bint: floating point array with its dimensions depend on those
;      of the measured source data
;
;
;Example
;-------
;
;Source = FINDGEN(25,25,3)
;nint = size(Source, /Dimensions)
;B = FLTARR(3,2,2,2)
;
;IDL> Bint = ffd_interpol(B, nint[0:2])

;--------------------------------------------------------------------------------
;    Copyright (C) 2017, Dimitra Flouri and Steven Sourbron
;
;    This program is free software; you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation; either version 2 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License along
;    with this program; if not, write to the Free Software Foundation, Inc.,
;    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
;--------------------------------------------------------------------------------


FUNCTION ffd_interpol, B, nint

  n = size(B, /dimensions)
  n = n[1:*]

  X = (n[0]-1E)*((((nint[0]-1E)-0.5-0.5)/(nint[0]-1E)) * findgen(nint[0]) + 0.5)/(nint[0]-1E)
  Y = (n[1]-1E)*((((nint[1]-1E)-0.5-0.5)/(nint[1]-1E)) * findgen(nint[1]) + 0.5)/(nint[1]-1E)
  Z = (n[2]-1E)*((((nint[2]-1E)-0.5-0.5)/(nint[2]-1E)) * findgen(nint[2]) + 0.5)/(nint[2]-1E)

  Bint = fltarr(3,nint[0],nint[1],nint[2],n[3])

  FOR k=0L, n[3]-1 DO BEGIN
  	Bint[0,*,*,*,k] = INTERPOLATE(reform(B[0,*,*,*,k]), X, Y, Z, /GRID)
  	Bint[1,*,*,*,k] = INTERPOLATE(reform(B[1,*,*,*,k]), X, Y, Z, /GRID)
  	Bint[2,*,*,*,k] = INTERPOLATE(reform(B[2,*,*,*,k]), X, Y, Z, /GRID)
  ENDFOR

  return, Bint

END

