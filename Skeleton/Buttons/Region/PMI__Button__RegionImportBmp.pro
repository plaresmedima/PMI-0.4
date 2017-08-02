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


pro PMI__Button__Event__RegionImportBmp, ev


;GET THE DIRECTORY CONTAINING THE FILES


    PMI__info, ev.top, State=s, Stdy=Stdy, Status=Status, Series=Series
    PMI__Message, status, 'Reading directory..'
    if not s->get_dir(title='Please select the folder containing the bmp data file', files=files, cnt=cnt) then return



;FIND ALL FILES WITH THE RIGHT FILE SIZE


	d = Series->d()
;	FOR i=0L,cnt-1 DO BEGIN
;		openr, 1, files[i], error=err
;		IF err NE 0 THEN BEGIN
;			files[i]=''
;		ENDIF ELSE BEGIN
;			s = fstat(1)
;			IF s.size NE d[0]*d[1] THEN files[i]=''
;		ENDELSE
;		close, 1
;	ENDFOR
;	valid = where(files ne '',cnt)
;	IF cnt EQ 0 THEN BEGIN
;    	ok = dialog_message(/information,'No files with the right size in this directory')
;    	goto, return
;	ENDIF
;	files = files[valid]


;GET THE FILES TO LOAD


	IF cnt LT d[2]*d[3] THEN BEGIN
    	ok = dialog_message(/information,['Not enough files in the folder'])
    	goto, return
	ENDIF

	sel = lindgen(cnt)
	WHILE cnt NE d[2]*d[3] DO BEGIN
		f = PMI__Form(ev.top, $
			Title='Please select the '  + strcompress(d[2]*d[3]) + ' files to load', $
			[ptr_new({Type:'LIST', Tag:'sel', Label:'Files', select:0L, Value:files})])
		IF f.cancel THEN goto, return
		sel = f.sel
		cnt = n_elements(sel)
		IF cnt NE d[2]*d[3] THEN BEGIN
    		ok = dialog_message(/information,/cancel,['Wrong number of files'])  ;add cancel
    		IF ok EQ 'Cancel' THEN goto, return
    	ENDIF
	ENDWHILE
	files = files[sel]


;LOAD FILES


	Region = Stdy->New('REGION', Name=fname(files[0]), Domain=Series->dom(), Color=Series->Clr(SAT='R'))
	for i=0L,d[2]-1 do begin
	for j=0L,d[3]-1 do begin
	   k = i*d[3] + j
       PMI__Message, Status, 'Importing BMP data', k/(d[2]*d[3]-1E)
       roi_ij = Read_Bmp(files[k])
       Region -> Write, Stdy->DataPath(), roi_ij/255B, i, j
    endfor
    endfor


    return: PMI__control, ev.top, /refresh
end


pro PMI__Button__Control__RegionImportBmp, id, v

	PMI__Info, tlb(id), Series=Series
	widget_control, id, sensitive = obj_valid(Series)
end

function PMI__Button__RegionImportBmp, parent

    return, widget_button(parent,  $
      	value      		= 'BMP',  $
      	event_pro    	= 'PMI__Button__Event__RegionImportBmp', $
    	pro_set_value 	= 'PMI__Button__Control__RegionImportBmp' )
end