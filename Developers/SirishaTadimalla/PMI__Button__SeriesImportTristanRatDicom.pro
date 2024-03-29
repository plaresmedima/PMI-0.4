

pro PMI__Button__SeriesImportTristanRatDicom_TimeMip, Stdy, Series

	nb = 4

	;CREATE OUTPUT

	U = ['a.u.']

	AUC = Stdy->New('SERIES',Default=Series,Name=Series->Name()+'[Area under the Curve ('+U+'*sec)]')	& AUC->t, Series->t(0)
	MAX = Stdy->New('SERIES',Default=Series,Name=Series->Name()+'[Time MIP ('+U+')]') 					& MAX->t, Series->t(0)

	MAX->Trim, 0E, 1
	AUC->Trim, 0E, 1

	;START OF CALCULATION

	time = Series->t()
	time = float(time-time[0])
	d = Series->d()

	MaxIm = fltarr(d[0]*d[1])
	AUCim = fltarr(d[0]*d[1])

	;LOOP OVER THE SLICES

	for i=0L,d[2]-1 do begin

		;calculate baseline P0
		P0 = Series -> Read(Stdy->DataPath(),i,0)
		for j=1L,nb-1 do P0 = P0 + Series -> Read(Stdy->DataPath(),i,j)
		P0 = P0/nb
		nozero = where(P0 NE 0, cnt_nozero)

		if cnt_nozero GT 0 then begin
		    for j=0L,d[3]-1 do begin

			    P = Series -> Read(Stdy->DataPath(),i,j)
				P = P[nozero]-P0[nozero]

			    if j eq 0 then begin
				    MaxIm[nozero] = P
			    endif else begin
				    ind = where(P gt MaxIm[nozero], cnt)
				    if cnt gt 0 then begin
					    MaxIm[nozero[ind]] = P[ind]
				    endif
				    AUCim[nozero] = AUCim[nozero] + (Time[j]-Time[j-1])*P
			    endelse
			endfor

        endif

		MAX -> Write, Stdy->DataPath(), MaxIm, i
		AUC -> Write, Stdy->DataPath(), AUCim, i

		MAX->Trim, max([MAX->Trim(1),max(MaxIm)]), 1
		AUC->Trim, max([AUC->Trim(1),max(AUCim)]), 1

        if cnt_nozero GT 0 then begin

		    MaxIm[nozero] = 0
		    AUCim[nozero] = 0
		endif

	endfor

	MAX->Trim, 0.4* MAX->Trim(1), 1
	AUC->Trim, 0.4* AUC->Trim(1), 1

end



pro PMI__Button__SeriesImportTristanRatDicom__Load, Stdy, files, status=status, cnt=cnt

	ProtocolName = PMI__Dicom__Read(files[0],'0018'x,'1030'x)

	nx = PMI__Dicom__Read(files[0],'0028'x,'0011'x)
	ny = PMI__Dicom__Read(files[0],'0028'x,'0010'x)


	scale_up = $
		strmid(ProtocolName,0,10) eq 'vFAIgFLASH' $
	 || strmid(ProtocolName,2,10) eq 'vFAIgFLASH' $
	 || strmid(ProtocolName,2,15) eq 'TRISTAN_IGFLASH' $
	 || strmid(ProtocolName,2,15) eq 'Tristan_IGFLASH'

	if scale_up then begin
		scalefactor = 2
		nx = floor(scalefactor*nx)
		ny = floor(scalefactor*ny)
	endif

	PMI__Message, status, 'Sorting DICOM Series ' + cnt

	z = PMI__Dicom__Read(files,'0020'x,'1041'x)
	t = PMI__Dicom__Read(files,'0020'x,'0013'x)

	files = PMI__Dicom__Sort(files, z, t)

	PMI__Message, status, 'Loading DICOM Series '+cnt

	t = t - min(t)
	case n_elements(t) of
		13: begin
			appendix = '_[VFA]'
			end
		30: begin
			appendix = '_[DCE]'
			t = 57*findgen(n_elements(t)) ;acquisition times
			end
		else: appendix = '_'
	endcase
	Name = string(ProtocolName) + appendix

	d = [nx,ny,n_elements(z),n_elements(t)]
	Dcm = Stdy -> New('SERIES',	Name = Name, Domain = {z:z, t:t, m:d[0:1]})

	range = [0E,0E]
	Info = objarr(d[2]*d[3])
	for k=0L,d[2]*d[3]-1 do begin

		PMI__Message, status, 'Loading DICOM Series '+cnt, k/(d[2]*d[3]-1.0)

		if files[k] ne '' then begin
			im = PMI__Dicom__ReadImage(files[k], Info=Info_k)
			if scale_up then im = congrid(im,d[0],d[1],interp=0)
			Info[k] = Info_k
			if size(im,/n_dimensions) ne 0 then begin
				range[0] = min([range[0],min(im,max=max)])
				range[1] = max([range[1],max])
			endif
			Dcm -> Write, Stdy->DataPath(), im, k
			im = im*0
		endif
	endfor

	Dcm -> Trim, [range[0],0.8*range[1]]
	Dcm -> ReadDicom, files[0]
	Dcm -> Set, obj_new('DATA_ELEMENT','5200'x,'9230'x,vr='SQ',value=Info)

	if appendix eq '_[DCE]' then PMI__Button__SeriesImportTristanRatDicom_TimeMip, Stdy, Dcm

end







function PMI__Button__SeriesImportTristanRatDicom__Input, top, files=files, first=first


    PMI__info, top, State=s, Status=Status
    if not s->get_dir(title='Please select the DICOM folder', files=files, cnt=n) then return, 0
	if n eq 0 then begin
		ok = dialog_message(/information,'Empty folder')
		return, 0
	endif

	IF NOT PMI__Dicom__QueryFolder(files, first, status=status, /imagedata) THEN RETURN, 0

	return, 1
end



pro PMI__Button__Event__SeriesImportTristanRatDicom, ev

	if not PMI__Button__SeriesImportTristanRatDicom__Input(ev.top, files=files, first=first) then goto, return

	PMI__info, ev.top, Stdy=Stdy, Status=Status

	n = n_elements(first)-1
	for i=0L,n-1 do $
		PMI__Button__SeriesImportTristanRatDicom__Load $
		, Stdy $
		, files[first[i]:first[i+1]-1] $
		, status = status $
		, cnt = strcompress(i+1,/remove_all) + '/' + strcompress(n,/remove_all)

	PMI__control, ev.top, /refresh, Path=Path
	return:PMI__Message, status
end





pro PMI__Button__Control__SeriesImportTristanRatDicom, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	widget_control, id, sensitive = obj_valid(Stdy)
end

function PMI__Button__SeriesImportTristanRatDicom, parent, value=value, separator=separator

	if n_elements(value) eq 0 then value = 'Import DICOM'

	return, widget_button(parent, value=value, separator=separator, $
		event_pro = 'PMI__Button__Event__SeriesImportTristanRatDicom', $
		pro_set_value = 'PMI__Button__Control__SeriesImportTristanRatDicom' )

end
