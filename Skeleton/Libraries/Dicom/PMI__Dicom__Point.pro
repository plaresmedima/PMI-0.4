;PURPOSE:

	;Places the file pointer just before the group tag of a given data element

;CALLING SEQUENCE:

	;Exists = PMI__Dicom__Point(unit, group, element, ImplicitVR=ImplicitVR)

;ARGUMENTS:

	;unit: LUN of a DICOM file open for reading
	;group, element: Group and element tag of the data element to find
	;Implicit VR: keyword, specifies if the transfer syntax uses implicit or explicit VR

;RETURN VALUES

	;Returns 0 if the data element with tags "group" and "element" does not exist
	;Returns 1 otherwise

;FILE POINTER POSITIONS

	;Initially: The file pointer must be positioned just before a group tag (or at end of file)
	;Upon return: the file pointer is positioned before the data element (group, element)
	;If the data element does not exist, the file pointer is placed before the next data element that exists
	;or at beginning of file if there is no next data element

;WRITTEN BY: Steven Sourbron
;DATE: 03-feb-2011


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


function PMI__Dicom__Point, unit, group, element, ts


;;;;Check the current position of the file pointer
;;;;If it is at the end of the file, or after the data element searched for, then rewind to the beginning

	ok = PMI__Dicom__NextTag(unit, gr, el, ts)
	if (gr gt group) or ((gr eq group) and (el gt element)) then begin
		point_lun, unit, 128
	    preamble = 'DICM'
	    readu, unit, preamble
	    if preamble ne 'DICM' then point_lun, unit, 0
		ok = PMI__Dicom__NextTag(unit, gr, el, ts)
	endif


;;;;Perform the search


	while 1B do begin

;;;;;;;;If the data element is found, then return 1

		if (gr eq group) and (el eq element) then return, 1B

;;;;;;;;Else skip the next data element
;;;;;;;;If the end of the file is reached, then position the pointer at the beginning and exit

		PMI__Dicom__SkipDataElement, unit, ts
		if not PMI__Dicom__NextTag(unit, gr, el, ts) then return, 0B

	endwhile

end