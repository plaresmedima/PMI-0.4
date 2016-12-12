;Reads a sequence of items (ie. the value field of a data element with value representation SQ)
;The file pointer should be positioned just before the group tag of the first item of the sequence
;Upon exit, the file pointer is placed before the next group tag


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


function PMI__Dicom__ReadSequence, unit, TransferSyntaxUID=ts, Template=Template, eof=eof



;   Value of an SQ data element is an array of objects of type HEADER
;   One HEADER object for each item in the sequence
;   If there are no items in the sequence, the array has one element with a NULL-Object



    value = objarr(1)



;   Read all items in the sequence



    eof = 1 - PMI__Dicom__NextTag(unit, gr, el, ts)
    while (gr eq 'FFFE'x) and (el eq 'E000'x) do begin
        Item = PMI__Dicom__ReadItem(unit, TransferSyntaxUID=ts, Template=Template)
        if obj_valid(value[0]) then value = [value,Item] else value[0] = Item
        eof = 1 - PMI__Dicom__NextTag(unit, gr, el, ts)
    endwhile




;   For a sequence of undefined length
;   Skip sequence delimitation item




    if (gr eq 'FFFE'x) and (el eq 'E0DD'x) then begin
        point_lun, -unit, p
        point_lun, unit, p+8
    endif


    return, value


end