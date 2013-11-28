;Converts DICOM countour data into a binary mask

;ARGUMENTS:

;CountourData: a 3*N string of coordinates for N contour points ('3006'x, '0050'x)
;ImagePosition: ;ImagePosition: Coordinates of upper left hand corner [0020,0032]
;PixelSpacing: Size of pixel in mm [0028,0030]
;SliceDimensions: Two-element array with slice dimensions (x,y) or (columns, rows)

;RETURN VALUE

;Binary image with dimensions equal to SliceDimensions


;
;    Copyright (C) 2013 Steven Sourbron
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


Function PMI__Dicom__Contour, ContourData, ImagePosition, PixelSpacing, SliceDimensions

	NumberOfContourPoints = n_elements(ContourData)/3
	Contour = lonarr(NumberOfContourPoints,2)
	for j=0L, NumberOfContourPoints-1 do begin
		PixelLocation = ContourData[3*j:3*(j+1)-2]
		Contour[j,*] = PMI__Dicom__SliceCoordinates(PixelLocation, ImagePosition, PixelSpacing, SliceDimensions[1])
	endfor
	subscripts = polyfillv(Contour[*,0],Contour[*,1],SliceDimensions[0],SliceDimensions[1])
	bin = bytarr(SliceDimensions[0],SliceDimensions[1])
	if n_elements(subscripts) gt 1 then bin[subscripts] = 1B

	return, bin
End