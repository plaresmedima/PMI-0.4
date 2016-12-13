


pro PMI__Button__SeriesImportDicom__LoadMultiFrame, status, cnt, files, NrOfFrames, Stdy

	;acq time image i (Philips): [5200,9230] > Item i: [2005,140F] > Item 0: [2005,10A0]

	PMI__Message, status, 'Loading DICOM Series '+cnt

	Manufacturer = PMI__Dicom__Read(files[0],'0008'x,'0070'x)

	n = n_elements(files)
	for i=0L,n-1 do begin
		Name 	= PMI__Dicom__Read(files[i],'0008'x,'103E'x, ok=okName)
		ny 		= PMI__Dicom__Read(files[i],'0028'x,'0010'x)
		nx 		= PMI__Dicom__Read(files[i],'0028'x,'0011'x)
		if Manufacturer Eq 'Philips Medical Systems ' $
			then nt = PMI__Dicom__Read(files[i],'2001'x,'1081'x) $	;number of time slots (Philips)
			else nt = PMI__Dicom__Read(files[i],'0054'x,'0071'x) 	;number of time slots (Generic)
		if NrOfFrames[i] gt 1 then begin
			if total(nt) lt 1 then nt=1 else nt = nt[0]
		endif else nt = 1L
		if nt gt NrOfFrames[i] then nt=NrOfFrames[i]
		nz = NrOfFrames[i]/nt
		if not okName then Name = 'Series'
		Dom = {z:findgen(nz), t:findgen(nt), m:[nx,ny]}
		Dcm = Stdy -> New('SERIES',	Name=Name, Domain=Dom)
		Dcm -> ReadDicom, files[i]

		if nz*nt eq NrOfFrames[i] then begin

			;Load data
			im = PMI__Dicom__Read(files[i],'7FE0'x,'0010'x)
			im = reform(im,nx,ny,nt,nz,/overwrite)
			if Manufacturer Eq 'Philips Medical Systems ' then begin  ;Get Acquisition times and rescale factors for Philips Multi-Frame data
				Groups = Dcm -> Get('5200'x,'9230'x)
				for l=0L,nt-1 do begin
					Seq = Groups[l] -> Get('2005'x,'140F'x) & AcqTime = Seq[0] -> Get('2005'x,'10A0'x)
					Dcm -> t, AcqTime, l
				endfor
				b = float(Seq[0] -> Get('2005'x,'100D'x)) ;Rescale factors are the same for all frames in Multi-Frame data
				a = float(Seq[0] -> Get('2005'x,'100E'x))
				im = (im - b)/a
			endif
			for l=0L,nt-1 do begin
				for k=0L,nz-1 do begin
					im_kl = float(reverse(im[*,*,l,k],2,/overwrite))
					Dcm -> Write, Stdy->DataPath(), im_kl, nz-1-k, l
				endfor
			endfor
			Dcm -> Trim, [min(im),max(im)]

			;Load header (only for Philips so far)
			if Manufacturer Eq 'Philips Medical Systems ' then begin
				Info = objarr(nt*nz) ;Create header
				for k=0L,nz-1 do begin
				for l=0L,nt-1 do begin
					m = k + l*nz
					Info[m] = Obj_new('HEADER')
					Seq = Groups[m] -> Get('0020'x,'9113'x) & Position = Seq[0] -> Get('0020'x,'0032'x)
					Seq = Groups[m] -> Get('0020'x,'9116'x) & Orientation = Seq[0] -> Get('0020'x,'0037'x)
					Seq = Groups[m] -> Get('0028'x,'9110'x) & PixelSpacing = Seq[0] -> Get('0028'x,'0030'x)
					Info[m] -> Set, obj_new('DATA_ELEMENT', '0020'x,'0032'x, vr='DS', value=Position)
					Info[m] -> Set, obj_new('DATA_ELEMENT', '0020'x,'0037'x, vr='DS', value=Orientation), /append
					Info[m] -> Set, obj_new('DATA_ELEMENT', '0028'x,'0030'x, vr='DS', value=PixelSpacing), /append
				endfor
				endfor
				Dcm -> Set, obj_new('DATA_ELEMENT','5200'x,'9230'x,vr='SQ',value=Info)
			endif

		endif else Dcm -> Name, Name + '[Incomplete]'
	endfor
end




pro PMI__Button__SeriesImportDicom__Load, Stdy, files, sort0, sort1, status=status, cnt=cnt


	;check if the series contains a multi-frame image
	;If yes,use the routine for multi-frame images
	n = n_elements(files)
	NrOfFrames = PMI__Dicom__Read(files,'0028'x,'0008'x)
	if total(NrOfFrames) gt n then begin
		PMI__Button__SeriesImportDicom__LoadMultiFrame, status, cnt, files, NrOfFrames, Stdy
		return
	endif

	PMI__Message, status, 'Sorting DICOM Series ' + cnt

	z = PMI__Dicom__Read(files,sort0[0],sort0[1])
	t = PMI__Dicom__Read(files,sort1[0],sort1[1])

	files = PMI__Dicom__Sort(files, z, t)

	PMI__Message, status, 'Loading DICOM Series '+cnt

	nx = PMI__Dicom__Read(files,'0028'x,'0011'x)
	ny = PMI__Dicom__Read(files,'0028'x,'0010'x)
	Name = PMI__Dicom__Read(files[0],'0018'x,'1030'x,ok=ok)
	if not ok then Name = 'Unknown'

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


function PMI__Button__TMUH_SeriesImportDicom__Input, top, files=files, first=first, sort0, sort1


;;;;SELECT A FOLDER
    PMI__info, top, State=s, Status=Status
    if not s->get_dir(title='Please select the DICOM folder', files=files, cnt=n) then return, 0
	if n eq 0 then begin
		ok = dialog_message(/information,'Empty folder')
		return, 0
	endif
	IF NOT PMI__Dicom__QueryFolder(files, first, status=status, /imagedata) THEN RETURN, 0


;;;;GET SORTING PARAMETERS
	sort0 = ['0020'x,'1041'x] 	;Slice location
	sort1 = ['0008'x,'0032'x]  	;Acquisition time


	return, 1
end




pro PMI__Button__Event__TMUH_SeriesImportDicom, ev

	if not PMI__Button__TMUH_SeriesImportDicom__Input(ev.top, files=files, first=first, sort0, sort1) then goto, return

	PMI__info, ev.top, Stdy=Stdy, Status=Status

	n=n_elements(first)-1
	for i=0L,n-1 do $
		PMI__Button__SeriesImportDicom__Load $
		, Stdy $
		, files[first[i]:first[i+1]-1] $
		, sort0,sort1 $
		, status = status $
		, cnt = strcompress(i+1,/remove_all) + '/' + strcompress(n,/remove_all)

	PMI__control, ev.top, /refresh, Path=Path
	return:PMI__Message, status
end





function PMI__Button__TMUH_SeriesImportDicom, parent, value=value, separator=separator

	if n_elements(value) eq 0 then value = 'Import DICOM'

	return, widget_button(parent, value=value, separator=separator, $
		event_pro = 'PMI__Button__Event__TMUH_SeriesImportDicom', $
		pro_set_value = 'PMI__Button__Control__SeriesImportDicom' )

end
