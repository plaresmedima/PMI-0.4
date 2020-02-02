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




pro PMI__Button__SeriesImportDicomSpecial__Load, Stdy, files, sort0, sort1, status=status, cnt=cnt


	PMI__Message, status, 'Sorting DICOM Series ' + cnt

	n = n_elements(files)
	z = make_array(n)
	for i=0L,n-1 do begin
		kk = PMI__Dicom__Read(files[i],sort0[0],sort0[1])
		z[i] = kk[1]; 1 for coronal, 2 for trans
	endfor
	t = PMI__Dicom__Read(files,sort1[0],sort1[1])

	files = PMI__Dicom__Sort(files, z, t)

	PMI__Message, status, 'Loading DICOM Series '+cnt

	nx = PMI__Dicom__Read(files,'0028'x,'0011'x)
	ny = PMI__Dicom__Read(files,'0028'x,'0010'x)

	StudyName 	= PMI__Dicom__Read(files[0],'0008'x,'1030'x)
	SeriesName 	= PMI__Dicom__Read(files[0],'0018'x,'1030'x)
	SeqName     = PMI__Dicom__Read(files[0],'0018'x,'0024'x)
	Name = string(StudyName) + ', ' + string(SeriesName)+ ', ' + string(SeqName)

;	Name = PMI__Dicom__Read(files[0],'0018'x,'1030'x,ok=ok)
;	if not ok then Name = 'Unknown'

	d = [max(nx),max(ny),n_elements(z),n_elements(t)]
	Dcm = Stdy -> New('SERIES',	Name = Name, Domain = {z:z, t:t, m:d[0:1]})

	x = (d[0]-nx)/2
	y = (d[1]-ny)/2

	im = fltarr(d[0],d[1])
	range = [0E,0E]
	Info = objarr(d[2]*d[3])
	for k=0L,d[2]*d[3]-1 do begin

		PMI__Message, status, 'Loading DICOM Series '+cnt, k/(d[2]*d[3]-1.0)

		if files[k] ne '' then begin
			image = PMI__Dicom__ReadImage(files[k], Info=Info_k)
			Info[k] = Info_k
			if size(image,/n_dimensions) ne 0 then begin
				im[x[k]:x[k]+nx[k]-1,y[k]:y[k]+ny[k]-1] = image
				range[0] = min([range[0],min(image,max=max)])
				range[1] = max([range[1],max])
			endif
			Dcm -> Write, Stdy->DataPath(), im, k
			im = im*0
		endif
	endfor

	Dcm -> Trim, [range[0],0.8*range[1]]
	Dcm -> ReadDicom, files[0]
	Dcm -> Set, obj_new('DATA_ELEMENT','5200'x,'9230'x,vr='SQ',value=Info)

end







function PMI__Button__SeriesImportDicomSpecial__Input, top, files=files, first=first, sort0, sort1


;;;;SELECT A FOLDER
    PMI__info, top, State=s, Status=Status
    if not s->get_dir(title='Please select the DICOM folder', files=files, cnt=n) then return, 0
	if n eq 0 then begin
		ok = dialog_message(/information,'Empty folder')
		return, 0
	endif

	IF NOT PMI__Dicom__QueryFolder(files, first, Descr=Descr, status=status, /imagedata) THEN RETURN, 0

;;;;PRESENT DESCRIPTION TO THE USER AND ASK FOR SELECTION
	sortdef = 'GE MEDICAL SYSTEMS' eq PMI__Dicom__Read(files[0],'0008'x,'0070'x)
	in = PMI__Form(top, Title='Please select which series to load', [$
		ptr_new({Type:'LIST', Label:'Series', select:0L,Value:Descr}), $
		ptr_new({Type:'DROPLIST', Label:'Sort by..', select:0L, Value:[ 'Slice location','Image number','Study date']}), $
		ptr_new({Type:'DROPLIST', Label:'..and by ', select:sortdef, Value:[ $
		   'Acquisition time','Acquisition Datetime', 'Image number','Temporal position identifier',$
		   'Series Number','Inversion Time','Echo Time','Repetition Time','Flip Angle','b-value (Siemens)' ]})])
	IF in.cancel THEN return, 0


;;;;GET SORTING PARAMETERS
	sort0 = [	['0020'x,'0032'x] $	;Slice location
			,	['0020'x,'0013'x] $ ;Image Number
			,	['0008'x,'0020'x] ] ;Study date
	sort1 = [	['0008'x,'0032'x] $ ;Acquisition time
			,	['0008'x,'002A'x] $ ;Acquisition Datetime
			,	['0020'x,'0013'x] $ ;Image Number
			,	['0020'x,'0100'x] $	;Temporal position identifier
			,	['0020'x,'0011'x] $	;Series Number
			,	['2005'x,'1572'x] $	;Inversion Time
			,   ['0018'x,'0081'x] $ ;Echo Time
			,	['0018'x,'0080'x] $	;Repetition Time
            ,	['0018'x,'1314'x] $; flip angle
            ,   ['0019'x,'100c'x] ] ;b-value (Siemens)
	sort0 = sort0[*,in.(1)]
	sort1 = sort1[*,in.(2)]



;;;;EXTRACT THE SELECTED SERIES
	sel = in.(0)
	nsel = n_elements(in.(0))
	ff = lonarr(nsel+1)
	nf = 0L
	for i=0L,nsel-1 do begin
		ff[i] = nf
		nf = nf + first[sel[i]+1] - first[sel[i]]
	endfor
	ff[nsel] = nf
	f = strarr(nf)
	for i=0L,nsel-1 do f[ff[i]:ff[i+1]-1] = files[first[sel[i]]:first[sel[i]+1]-1]
  	files = f
  	first = ff


	return, 1
end










pro PMI__Button__Event__SeriesImportDicomSpecial, ev

	if not PMI__Button__SeriesImportDicomSpecial__Input(ev.top, files=files, first=first, sort0, sort1) then goto, return

	PMI__info, ev.top, Stdy=Stdy, Status=Status

	n=n_elements(first)-1
	for i=0L,n-1 do $
		PMI__Button__SeriesImportDicomSpecial__Load $
		, Stdy $
		, files[first[i]:first[i+1]-1] $
		, sort0,sort1 $
		, status = status $
		, cnt = strcompress(i+1,/remove_all) + '/' + strcompress(n,/remove_all)

	PMI__control, ev.top, /refresh, Path=Path
	return:PMI__Message, status
end





pro PMI__Button__Control__SeriesImportDicomSpecial, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	widget_control, id, sensitive = obj_valid(Stdy)
end

function PMI__Button__SeriesImportDicomSpecial, parent, value=value, separator=separator

	if n_elements(value) eq 0 then value = 'Import DICOM Special'

	return, widget_button(parent, value=value, separator=separator, $
		event_pro = 'PMI__Button__Event__SeriesImportDicomSpecial', $
		pro_set_value = 'PMI__Button__Control__SeriesImportDicomSpecial' )

end
