;Converts a PixelLocation given in DICOM coordinates into PMI coordinates

;ARGUMENTS:

;PixelLocation: pixel location [x,y] in DICOM coordinates (mm)
;ImagePosition: Coordinates of upper left hand corner [0020,0032]
;PixelSpacing: Size of pixel in mm [0028,0030]
;Rows: Nr of Rows in the image [0028,0010]

;RETURN VALUE

;Pixel location (in units of pixels) with origin in lower left-had corner


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


FUNCTION PMI__Dicom__SliceCoordinates, PixelLocation, ImagePosition, PixelSpacing, Rows

	x = PixelLocation[0] - ImagePosition[0]
	y = ImagePosition[1] + (Rows-1)*PixelSpacing[1] - PixelLocation[1]

	return, floor([x,y]/PixelSpacing - 0.5)
END