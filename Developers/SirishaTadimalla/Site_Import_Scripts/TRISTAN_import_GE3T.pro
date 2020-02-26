
pro TRISTAN_Import_GE3T__LoadDynamics_MoCo, Sequence, Stdy, files

	; Process dynamic dataset
	z = PMI__Dicom__Read(files,'0020'x,'1041'x) ;Array of Slice locations
	t = PMI__Dicom__Read(files,'0008'x,'0032'x) ;Array of Acquisition times
	files = PMI__Dicom__Sort(files, z, t) ; returns a 2D string array of file names

	nx = PMI__Dicom__Read(files[0],'0028'x,'0011'x)  ;nr of rows
	ny = PMI__Dicom__Read(files[0],'0028'x,'0010'x)  ;nr of columns
	nz = n_elements(z)
	nt = n_elements(t)
	d = [nx,ny,nz,nt] ;dimensions of the series

	image = fltarr(nx,ny,nz,nt) ;array holding the data

	for k=0L,d[3]-1 do begin ;loop over all slices and times
		for j=0L,d[2]-1 do begin
			ind = j + d[2]*k
			image[*,*,j,k] = PMI__Dicom__ReadImage(files[ind]) ;read image data from file
		endfor
	endfor

	;;;;;; Perform motion correction
	; Set parameters for MoCoMo
	in = {res:1E, prec:1E}
	; Set window
	win = {p:[0L,0L,0L], n:d[0:2]}

	; Calculate
	image = TRANSPOSE(image, [3,0,1,2])
	image = MOCOMO_3D(image, 'QIM_CONST', 0B, in.res, in.prec, Win=win)
	image = TRANSPOSE(image, [1,2,3,0])

	;;;;;; Write series
	Dcm = Stdy -> New('SERIES', Name=Sequence+'[Motion-free]', Domain = {z:z, t:t, m:[nx,ny]}) ;new series object in display
	Dcm -> Write, Stdy->DataPath(), image  ;write raw data to disk
	Dcm -> Trim, [min(image),0.8*max(image)]  ;set default grey scale ranges
	Dcm -> ReadDicom, files[0]  ;read DICOM header information for the series
end

pro TRISTAN_Import_GE3T__LoadVFA_MoCo, Sequence, Stdy, files

	;;;;;;; Initialise
	nx = PMI__Dicom__Read(files,'0028'x,'0011'x)
	ny = PMI__Dicom__Read(files,'0028'x,'0010'x)
	nz = 56
	nt = 12

	FA = [2,5,10,15,20,25]
	nFA = n_elements(FA)

	vfa = fltarr(max(nx),max(ny),nz,nFA) ;array holding the data
	image = fltarr(max(nx),max(ny),nz,nt) ;array holding the data
	for n = 0L, nFA-1 do begin
		;j = where(PMI__Dicom__Read(files,'0018'x,'1314'x) eq FA[n])
		m = nFA*(indgen(nz*nt))+n
		files_FA = files[m]
		z = PMI__Dicom__Read(files_FA,'0020'x,'1041'x) ;Array of Slice locations
		t = PMI__Dicom__Read(files_FA,'0020'x,'0013'x) ;Array of Acquisition times

		files_FA = PMI__Dicom__Sort(files_FA, z, t) ; returns a 2D string array of file names
		d = [max(nx),max(ny),nz,nt] ;dimensions of the series

		for k=0L,d[3]-1 do begin ;loop over all slices and times
			for j=0L,d[2]-1 do begin
				ind = j + d[2]*k
				image[*,*,j,k] = PMI__Dicom__ReadImage(files_FA[ind]) ;read image data from file
			endfor
		endfor

		;;;;;; Perform motion correction
		; Set parameters for MoCoMo
		in = {res:1E, prec:1E}
		; Set window
		win = {p:[0L,0L,0L], n:d[0:2]}

		; Calculate
		image = TRANSPOSE(image, [3,0,1,2])
		image = MOCOMO_3D(image, 'QIM_CONST', 0B, in.res, in.prec, Win=win)
		image = TRANSPOSE(image, [1,2,3,0])
		; Calculate mean over time
		for j=0L, d[2]-1 do begin ; each slice
			im = reform(image[*,*,j,*])
			vfa[*,*,j,n] = TOTAL(im,3)/nt
		endfor
		image = 0*image
	endfor

	; Calculate T1 map using linear fit
	TR = PMI__Dicom__Read(files[0],'0018'x,'0080'x) ;msec
	Dom = {z:z, t:t[0], m:d[0:1]}
    S0_series = Stdy->New('SERIES', Domain= Dom,  Name= Sequence+'_VFA_S0')
    R1_series = Stdy->New('SERIES', Domain= Dom,  Name= Sequence+'_VFA_R1 (ms-1)')
    T1_series = Stdy->New('SERIES', Domain= Dom,  Name= Sequence+'_VFA_T1 (ms)')
    FIT_series = Stdy->New('SERIES', Domain= Dom,  Name= Sequence+'_VFA_RMS (%)')

	SVA_slice = fltarr(nFA,d[0]*d[1])
	S0_slice = fltarr(d[0]*d[1])
	R1_slice = fltarr(d[0]*d[1])
	T1_slice = fltarr(d[0]*d[1])
	FIT_slice = fltarr(d[0]*d[1])

	for j=0L,d[2]-1 do begin ;loop over slices
		PMI__Message, status, 'Calculating T1 map ', j/(d[2]-1E)

		for k=0L,nFA-1 do SVA_slice[k,*] = vfa[*,*,j,k]

		for i=0L,d[0]*d[1]-1 do begin

			PAR = VFA_Linear_T1fit(TR, FA, reform(SVA_slice[*,i]), RMS = rms)

			S0_slice[i] = PAR[1]
			R1_slice[i] = PAR[0]
			T1_slice[i] = 1/PAR[0]
			Fit_slice[i] = rms

		endfor

		S0_series->Write, Stdy->DataPath(), S0_slice, j
		R1_series->Write, Stdy->DataPath(), R1_slice, j
		T1_series->Write, Stdy->DataPath(), T1_slice, j
		FIT_series->Write, Stdy->DataPath(), Fit_slice, j

	endfor

end

pro TRISTAN_Import_GE3T__LoadDynamics, Sequence, Stdy, files

	z = PMI__Dicom__Read(files,'0020'x,'1041'x) ;Array of Slice locations
	t = PMI__Dicom__Read(files,'0008'x,'0032'x) ;Array of Acquisition times
	files = PMI__Dicom__Sort(files, z, t) ; returns a 2D string array of file names

	nx = PMI__Dicom__Read(files[0],'0028'x,'0011'x)  ;nr of rows
	ny = PMI__Dicom__Read(files[0],'0028'x,'0010'x)  ;nr of columns
	nz = n_elements(z)
	nt = n_elements(t)

	d = [nx,ny,nz,nt] ;dimensions of the series

	image = fltarr(nx,ny,nz,nt) ;array holding the data

	for k=0L,d[3]-1 do begin ;loop over all slices and times
		for j=0L,d[2]-1 do begin
			ind = j + d[2]*k
			image[*,*,j,k] = PMI__Dicom__ReadImage(files[ind]) ;read image data from file
		endfor
	endfor

    Dcm = Stdy -> New('SERIES', Name=Sequence, Domain = {z:z, t:t, m:[nx,ny]}) ;new series object in display
	Dcm -> Write, Stdy->DataPath(), image  ;write raw data to disk
	Dcm -> Trim, [min(image),0.8*max(image)]  ;set default grey scale ranges
	Dcm -> ReadDicom, files[0]  ;read DICOM header information for the series

end

pro TRISTAN_Import_GE3T__LoadVFA, Sequence, Stdy, files

	sort_z = ['0020'x,'1041'x]
	sort_t = ['0020'x,'0013'x]
	z = PMI__Dicom__Read(files,sort_z[0],sort_z[1])
  	t = PMI__Dicom__Read(files,sort_t[0],sort_t[1])

  	files = PMI__Dicom__Sort(files, z, t)

  	nx = PMI__Dicom__Read(files,'0028'x,'0011'x)
  	ny = PMI__Dicom__Read(files,'0028'x,'0010'x)

	FA = [2,5,10,15,20,25]

  	d = [max(nx),max(ny),n_elements(z),n_elements(FA)]

	image = fltarr(max(nx),max(ny),n_elements(z),n_elements(FA)) ;array holding the data
	for k=0L,d[3]-1 do begin ;loop over all slices and FAs
		for j=0L,d[2]-1 do begin
			ind = j + d[2]*k
			image[*,*,j,k] = PMI__Dicom__ReadImage(files[ind]) ;read image data from file
		endfor
	endfor

	; Calculate T1 map using linear fit
	TR = PMI__Dicom__Read(files[0],'0018'x,'0080'x) ;msec
	nFA = n_elements(FA)

	Dom = {z:z, t:t[0], m:d[0:1]}
    S0_series = Stdy->New('SERIES', Domain= Dom,  Name= Sequence+'_VFA_S0')
    R1_series = Stdy->New('SERIES', Domain= Dom,  Name= Sequence+'_VFA_R1 (ms-1)')
    T1_series = Stdy->New('SERIES', Domain= Dom,  Name= Sequence+'_VFA_T1 (ms)')
    FIT_series = Stdy->New('SERIES', Domain= Dom,  Name= Sequence+'_VFA_RMS (%)')

	SVA_slice = fltarr(nFA,d[0]*d[1])
	S0_slice = fltarr(d[0]*d[1])
	R1_slice = fltarr(d[0]*d[1])
	T1_slice = fltarr(d[0]*d[1])
	FIT_slice = fltarr(d[0]*d[1])

	for j=0L,d[2]-1 do begin ;loop over slices

		PMI__Message, status, 'Calculating T1 map ', j/(d[2]-1E)

		for k=0L,nFA-1 do SVA_slice[k,*] = image[*,*,j,k]

		for i=0L,d[0]*d[1]-1 do begin

			PAR = VFA_Linear_T1fit(TR, FA, reform(SVA_slice[*,i]), RMS = rms)

			S0_slice[i] = PAR[1]
			R1_slice[i] = Par[0]
			T1_slice[i] = 1/Par[0]
			Fit_slice[i] = rms

		endfor

		S0_series->Write, Stdy->DataPath(), S0_slice, j
		R1_series->Write, Stdy->DataPath(), R1_slice, j
		T1_series->Write, Stdy->DataPath(), T1_slice, j
		FIT_series->Write, Stdy->DataPath(), Fit_slice, j

	endfor
end


pro TRISTAN_Import_GE3T__Load3DSPGR_BHdyn, Sequence, Stdy, files, status

	PMI__Message, status, 'Loading 3DSPGR BH Data'
	Series = string(PMI__Dicom__Read(files,'0008'x,'103E'x))
	i = where(strmatch(Series, Sequence+'*') eq 1,cnt)
	if cnt eq 0 then return

	files_BH = files[i]

	; Load dynamic BH images - no motion correction
	TRISTAN_Import_GE3T__LoadDynamics, Sequence+'_dynamic', Stdy, files_BH

end

pro TRISTAN_Import_GE3T__Load3DSPGR_BH, Sequence, Stdy, files, status

	PMI__Message, status, 'Loading 3DSPGR BH Data'
	Series = string(PMI__Dicom__Read(files,'0008'x,'103E'x))
	i = where(strmatch(Series, Sequence+'*') eq 1,cnt)
	if cnt eq 0 then return

	files_BH = files[i]

	; Get VFA T1 map from BH dataset - no motion correction
	TRISTAN_Import_GE3T__LoadVFA, Sequence, Stdy, files_BH

end

pro TRISTAN_Import_GE3T__Load3DSPGR_FBdyn, Sequence, Stdy, files, status

	PMI__Message, status, 'Loading 3DSPGR FB dynamic Data'
	Series = string(PMI__Dicom__Read(files,'0008'x,'103E'x))
	i = where(strmatch(Series, Sequence+'*') eq 1,cnt)
	if cnt eq 0 then return
	files_FB = files[i]

	; Load dynamic FB images - after motion correction
	TRISTAN_Import_GE3T__LoadDynamics_MoCo, Sequence+'_dynamic', Stdy, files_FB

end

pro TRISTAN_Import_GE3T__Load3DSPGR_FB, Sequence, Stdy, files, status

	PMI__Message, status, 'Loading 3DSPGR FB Data'
	Series = string(PMI__Dicom__Read(files,'0008'x,'103E'x))
	i = where(strmatch(Series, Sequence+'*') eq 1,cnt)
	if cnt eq 0 then return
	files_FB = files[i]
	print, i[0]
	; Get VFA T1 map from FB dataset - after motion correction
	TRISTAN_Import_GE3T__LoadVFA_MoCo, Sequence, Stdy, files_FB

end


pro TRISTAN_Import_GE3T__LoadVolume, Sequence, Stdy, files, status

	PMI__Message, status, 'Loading ' + Sequence
	Series = string(PMI__Dicom__Read(files,'0008'x,'103E'x))
	i = where(Series eq Sequence, cnt)


	if cnt eq 0 then return
	files_VOL = files[i]  ;Array of filenames for the Sequence to be imported

	z = PMI__Dicom__Read(files_VOL,'0020'x,'1041'x) ;Array of Slice locations
	t = PMI__Dicom__Read(files_VOL,'0008'x,'0032'x) ;Array of Acquisition times
	files_VOL = PMI__Dicom__Sort(files_VOL, z, t) ; returns a 2D string array of file names

	nx = PMI__Dicom__Read(files_VOL[0],'0028'x,'0011'x)  ;nr of rows
	ny = PMI__Dicom__Read(files_VOL[0],'0028'x,'0010'x)  ;nr of columns
	nz = n_elements(z)
	nt = n_elements(t)

	d = [nx,ny,nz,nt] ;dimensions of the series
	Dcm = Stdy -> New('SERIES', Name=Sequence, Domain = {z:z, t:t, m:[nx,ny]}) ;new series object in display
	image = fltarr(nx,ny,nz*nt) ;array holding the data

	for k=0L,d[2]*d[3]-1 do begin ;loop over all slices and times

	  PMI__Message, status, 'Loading ' + Sequence, k/(d[2]*d[3]-1.0)
	  image[*,*,k] = PMI__Dicom__ReadImage(files_VOL[k]) ;read image data from file

	endfor

	Dcm -> Write, Stdy->DataPath(), image  ;write raw data to disk
	Dcm -> Trim, [min(image),0.8*max(image)]  ;set default grey scale ranges
	Dcm -> ReadDicom, files_VOL[0]  ;read DICOM header information for the series

end





pro TRISTAN_Import_GE3T__LoadSequence, Sequence, Stdy, files, first, status

  n = n_elements(first)-1

  Series = string(PMI__Dicom__Read(files,'0008'x,'103E'x))
  i = where(Series eq Sequence, cnt)
  if cnt eq 0 then return
  Series = Series[i]
  files_SEQ = files[i]

  PMI__Message, status, 'Sorting ' + Sequence

  sort_z = ['0020'x,'1041'x]
  sort_t = ['0008'x,'0032'x]

  z = PMI__Dicom__Read(files_SEQ,sort_z[0],sort_z[1])
  t = PMI__Dicom__Read(files_SEQ,sort_t[0],sort_t[1])
  files_SEQ = PMI__Dicom__Sort(files_SEQ, z, t)

  nx = PMI__Dicom__Read(files_SEQ,'0028'x,'0011'x)
  ny = PMI__Dicom__Read(files_SEQ,'0028'x,'0010'x)

  d = [max(nx),max(ny),n_elements(z),n_elements(t)]
  Dcm = Stdy -> New('SERIES',	Name = Sequence, Domain = {z:z, t:t, m:d[0:1]})

  x = (d[0]-nx)/2
  y = (d[1]-ny)/2

  im = fltarr(d[0],d[1])
  range = [0E,0E]
  for k=0L,d[2]*d[3]-1 do begin

	PMI__Message, status, 'Loading ' + Sequence, k/(d[2]*d[3]-1.0)

	image = PMI__Dicom__ReadImage(files_SEQ[k])
	if size(image,/n_dimensions) ne 0 then begin
		im[x[k]:x[k]+nx[k]-1,y[k]:y[k]+ny[k]-1] = image
		range[0] = min([range[0],min(image,max=max)])
		range[1] = max([range[1],max])
	endif
	Dcm -> Write, Stdy->DataPath(), im, k
	im = im*0

  endfor

  Dcm -> Trim, [range[0],0.8*range[1]]
  Dcm -> ReadDicom, files_SEQ[0]


end


pro TRISTAN_Import_GE3T, Stdy, files, first, status=status

;;;;CHECK SEQUENCE PARAMETERS

;;;;LOAD THE SERIES

	seq1 = '3 plane Loc BH' ; All Localiser images bundled up

	seq2 = 'Cor SSFSE NPW BH' ; Coronal T2 HASTE

	seq3 = 'VFA_FB'  ; 3D SPGR - VFA FB
	seq4 = 'VFA_BH'  ; 3D SPGR - VFA BH
	seq5 = '15_FB_DYN'  ; 3D SPGR - dynamic FB
	seq6 = '15_BH_DYN'  ; 3D SPGR - dynamic BH

	TRISTAN_Import_GE3T__Load3DSPGR_FB, seq3, Stdy, files, status
	TRISTAN_Import_GE3T__Load3DSPGR_BH, seq4, Stdy, files, status
	;TRISTAN_Import_GE3T__Load3DSPGR_FBdyn, seq5, Stdy, files, status
	;TRISTAN_Import_GE3T__Load3DSPGR_BHdyn, seq6, Stdy, files, status

	;TRISTAN_Import_GE3T__LoadSequence, seq1, Stdy, files, first, status

	;TRISTAN_Import_GE3T__LoadVolume, seq2, Stdy, files, status

end