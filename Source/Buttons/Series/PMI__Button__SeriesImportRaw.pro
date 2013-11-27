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


pro PMI__Button__SeriesImportRaw__Load, in, Stdy, file, Status, type

	Dom = { z:findgen(in.z), t:findgen(in.t), m:[in.x,in.y] }
    New = Stdy->New('SERIES',Name=fname(file),Domain=Dom)
    d = New->d()

    if in.sort eq 1 then Dim = [d[3],d[2]] else Dim = [d[2],d[3]]

    for k=0L,d[2]*d[3]-1 do begin

       PMI__Message, Status, 'Importing RAW data', k/(d[2]*d[3]-1E)

       slice = PMI__ReadData(file,type,position=k*d[0]*d[1],length=d[0]*d[1])

       mins = min(slice,max=maxs)
       if k eq 0 then Trim = [mins,maxs] else begin
         Trim[0] = min([Trim[0],mins])
         Trim[1] = max([Trim[1],maxs])
       endelse

       r = reform_ind(Dim,ind=k)
       if in.sort eq 1 then r = [r[1],r[0]]

       New -> Write, Stdy->DataPath(), float(slice), r[0], r[1]
    endfor

    New -> Trim, float(Trim)
end

pro PMI__Button__Event__SeriesImportRaw, ev

;GET THE DIRECTORY CONTAINING THE FILES

    PMI__info, ev.top, State=s, Stdy=Stdy, Status=Status
    if not s->get_dir(title='Please select the folder containing the raw data file', files=files, cnt=cnt) then return
    if cnt eq 0 then begin
    	ok = dialog_message(/information,'No data in this folder')
    	goto, return
    endif

;GET THE PROPERTIES OF THE DATA

	SortTypes = ['Time 0, Time 1, ...','Slice 0, Slice 1, ...']
	DataTypes = ['Byte (8-bit)','Integer (16-bit)','Longword (32-bit)','Float (32-bit)','Double (64-bit)','Unsigned integer (16-bit)','Unsigned long (32-bit)','Long integer (64-bit)','Unsigned long integer (64-bit)']
	TypeCodes = [1,2,3,4,5,12,13,14,15]
	in = PMI__Form(ev.top, Title='Please define the data properties', [$
		ptr_new({Type:'DROPLIST', Tag:'sort', Label:'Sorting', select:0L, Value:SortTypes}), $
		ptr_new({Type:'DROPLIST', Tag:'type', Label:'Data type', select:1L, Value:DataTypes}), $
		ptr_new({Type:'VALUE',Tag:'x' , Label:'x-dimension', Value:128L}),$
		ptr_new({Type:'VALUE',Tag:'y' , Label:'y-dimension', Value:128L}),$
		ptr_new({Type:'VALUE',Tag:'z' , Label:'z-dimension', Value:20L}),$
		ptr_new({Type:'VALUE',Tag:'t' , Label:'t-dimension', Value:60L}) $
		])
	IF in.cancel THEN goto, return
	type = TypeCodes[in.type]
	if in.x le 0 then in.x=1
	if in.y le 0 then in.y=1
	if in.z le 0 then in.z=1
	if in.t le 0 then in.t=1

;FIND ALL FILES WITH THE RIGHT FILE SIZE

	nbytes = in.x*in.y*in.z*in.t*PMI__nbytes(type)
	FOR i=0L,cnt-1 DO BEGIN
		openr, 1, files[i], error=err
		IF err NE 0 THEN BEGIN
			files[i]=''
		ENDIF ELSE BEGIN
			s = fstat(1)
			IF s.size NE nbytes THEN files[i]=''
		ENDELSE
		close, 1
	ENDFOR
	valid = where(files ne '',cnt)
	IF cnt EQ 0 THEN BEGIN
    	ok = dialog_message(/information,'No files with the right size in this directory')
    	goto, return
	ENDIF
	files = files[valid]

;GET THE FILES TO LOAD

	f = PMI__Form(ev.top, Title='Please select which files to load', [$
		ptr_new({Type:'LIST', Tag:'sel', Label:'Files', select:0L, Value:files}) $
		])
	IF f.cancel THEN goto, return
	files = files[f.sel]
	FOR i=0L,n_elements(files)-1 DO PMI__Button__SeriesImportRaw__Load, in, stdy, files[i], status, type

    return: PMI__control, ev.top, /refresh
end


pro PMI__Button__Control__SeriesImportRaw, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	widget_control, id, sensitive = obj_valid(Stdy)
end

function PMI__Button__SeriesImportRaw, parent

    return, widget_button(parent  $
    ,  	value      		= 'RAW'  $
    ,  	event_pro    	= 'PMI__Button__Event__SeriesImportRaw' $
    ,	pro_set_value 	= 'PMI__Button__Control__SeriesImportRaw' $
    )
end