
pro TRISTAN_Import_Siemens3T__LoadDynamics_MoCo, Sequence, Stdy, files

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

pro TRISTAN_Import_Siemens3T__LoadVFA_MoCo, Sequence, Stdy, files

	;;;;;;; Initialise
	nx = PMI__Dicom__Read(files,'0028'x,'0011'x)
	ny = PMI__Dicom__Read(files,'0028'x,'0010'x)
	nz = 36
	nt = 36

	FA = PMI__Dicom__Read(files,'0018'x,'1314'x)
	FA = FA[UNIQ(FA)]
	nFA = n_elements(FA)

	vfa = fltarr(max(nx),max(ny),nz,nFA) ;array holding the data
	image = fltarr(max(nx),max(ny),nz,nt) ;array holding the data
	for n = 0L, nFA-1 do begin
		j = where(PMI__Dicom__Read(files,'0018'x,'1314'x) eq FA[n])
		files_FA = files[j]
		z = PMI__Dicom__Read(files_FA,'0020'x,'1041'x) ;Array of Slice locations
		t = PMI__Dicom__Read(files_FA,'0008'x,'0032'x) ;Array of Acquisition times

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

pro TRISTAN_Import_Siemens3T__LoadDynamics, Sequence, Stdy, files

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

pro TRISTAN_Import_Siemens3T__LoadVFA, Sequence, Stdy, files

	sort_z = ['0020'x,'1041'x]
	sort_t = ['0008'x,'0032'x]
	z = PMI__Dicom__Read(files,sort_z[0],sort_z[1])
  	t = PMI__Dicom__Read(files,sort_t[0],sort_t[1])

  	files = PMI__Dicom__Sort(files, z, t)

  	nx = PMI__Dicom__Read(files,'0028'x,'0011'x)
  	ny = PMI__Dicom__Read(files,'0028'x,'0010'x)

	FA = PMI__Dicom__Read(files,'0018'x,'1314'x)
	FA = FA[UNIQ(FA)]

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

pro TRISTAN_Import_Siemens3T__Load3DSPGR, Sequence, Stdy, files, Series, status

	PMI__Message, status, 'Loading 3DSPGR Data'

	i = where(Series eq Sequence, cnt)

	if cnt eq 0 then return

	files_all = files[i]

	; Separate the breath-hold and free-breathing series using number of rows
	jBH = where(PMI__Dicom__Read(files_all,'0028'x,'0010'x) eq 256)
	jFB = where(PMI__Dicom__Read(files_all,'0028'x,'0010'x) eq 96)

	files_BH = files_all[jBH]
	files_FB = files_all[jFB]

	;;;;;;; Process BH dataset
	; Get only VFA images
	seriesNumbers = PMI__Dicom__Read(files_BH,'0020'x,'0011'x)
	uniqueSeries = seriesNumbers[UNIQ(seriesNumbers, SORT(seriesNumbers))]
	j1 = where(PMI__Dicom__Read(files_BH,'0020'x,'0011'x) NE uniqueSeries[n_elements(uniqueSeries)-1])

	; Get VFA T1 map from BH dataset - no motion correction
	TRISTAN_Import_Siemens3T__LoadVFA, Sequence+'_BH', Stdy, files_BH[j1]

	; Load dynamic BH images - no motion correction
	j2 = where(PMI__Dicom__Read(files_BH,'0020'x,'0011'x) eq uniqueSeries[n_elements(uniqueSeries)-1])
	TRISTAN_Import_Siemens3T__LoadDynamics, Sequence+'_dynamicBH', Stdy, files_BH[j2]

	;;;;;;;; Process FB dataset
	; Get only VFA images
	seriesNumbers = PMI__Dicom__Read(files_FB,'0020'x,'0011'x)
	uniqueSeries = seriesNumbers[UNIQ(seriesNumbers, SORT(seriesNumbers))]
	j3 = where(PMI__Dicom__Read(files_FB,'0020'x,'0011'x) NE uniqueSeries[n_elements(uniqueSeries)-1])

	; Get VFA T1 map from FB dataset - after motion correction
	TRISTAN_Import_Siemens3T__LoadVFA_MoCo, Sequence+'_FB', Stdy, files_FB[j3]

	; Load dynamic FB images - after motion correction
	j4 = where(PMI__Dicom__Read(files_FB,'0020'x,'0011'x) eq uniqueSeries[n_elements(uniqueSeries)-1])
	TRISTAN_Import_Siemens3T__LoadDynamics_MoCo, Sequence+'_dynamicFB', Stdy, files_FB[j4]

end

pro TRISTAN_Import_Siemens3T__LoadRAVE, Sequence, Stdy, files, Series, status

	PMI__Message, status, 'Loading RAVE Data'

	i = where(Series eq Sequence, cnt)
	if cnt eq 0 then return
	files_RAVE = files[i]

	; Separate individual flip angle and dynamic series using the series numbers
	seriesNumbers = PMI__Dicom__Read(files_RAVE,'0020'x,'0011'x)
	uniqueSeries = seriesNumbers[UNIQ(seriesNumbers, SORT(seriesNumbers))]
	j = where(PMI__Dicom__Read(files_RAVE,'0020'x,'0011'x) NE uniqueSeries[n_elements(uniqueSeries)-1])
	files_SEQ = files_RAVE[j]


	; Get VFA T1 map from BH dataset - no motion correction
	TRISTAN_Import_Siemens3T__LoadVFA, Sequence+'_RAVE', Stdy, files_SEQ

	; Get dynamic dataset
	j = where(PMI__Dicom__Read(files_RAVE,'0020'x,'0011'x) eq uniqueSeries[n_elements(uniqueSeries)-1])
	files_SEQ = files_RAVE[j]
	TRISTAN_Import_Siemens3T__LoadDynamics, Sequence+'_dynamicRAVE', Stdy, files_SEQ


end



pro TRISTAN_Import_Siemens3T__LoadInversionRecovery, Sequence, Stdy, files, Series, status

	PMI__Message, status, 'Loading Inversion Recovery Data'

	i = where(Series eq Sequence, cnt)
	if cnt eq 0 then return
	files_IR = files[i]

	; Get phase images
	j = where(PMI__Dicom__Read(files_IR,'0008'x,'0008'x) eq 'ORIGINAL\PRIMARY\P\DIS2D')

	files_P = files_IR[j]

	z = PMI__Dicom__Read(files_P,'0020'x,'1041'x) ;Slice location
	t = PMI__Dicom__Read(files_P,'0018'x,'0082'x) ;Inversion Time

	files_TI = PMI__Dicom__Sort(files_P, z, t)

	nx = PMI__Dicom__Read(files_TI[0],'0028'x,'0011'x)
	ny = PMI__Dicom__Read(files_TI[0],'0028'x,'0010'x)
	nz = n_elements(z)
	nt = n_elements(t)

	imageP = fltarr(nx,ny,nz*nt)

	for k=0L,nz*nt-1 do imageP[*,*,k] = PMI__Dicom__ReadImage(files_TI[k])


	Dcm = Stdy -> New('SERIES', Name=Sequence+'_Phase', Domain = {z:z, t:t, m:[nx,ny]})
	Dcm -> Write, Stdy->DataPath(), imageP
	Dcm -> Trim, [min(imageP),0.8*max(imageP)]
	Dcm -> ReadDicom, files_TI[0]

	; Get magnitude images
	j = where(PMI__Dicom__Read(files_IR,'0008'x,'0008'x) eq 'ORIGINAL\PRIMARY\M\DIS2D')

	files_M = files_IR[j]

	z = PMI__Dicom__Read(files_M,'0020'x,'1041'x) ;Slice location
	t = PMI__Dicom__Read(files_M,'0018'x,'0082'x) ;Inversion Time

	files_TI = PMI__Dicom__Sort(files_M, z, t)
	t = t[sort(t)]

	nx = PMI__Dicom__Read(files_TI[0],'0028'x,'0011'x)
	ny = PMI__Dicom__Read(files_TI[0],'0028'x,'0010'x)
	nz = n_elements(z)
	nt = n_elements(t)

	imageM = fltarr(nx,ny,nz*nt)

	for k=0L,nz*nt-1 do imageM[*,*,k] = PMI__Dicom__ReadImage(files_TI[k])


	Dcm = Stdy -> New('SERIES', Name=Sequence+'_Magnitude', Domain = {z:z, t:t, m:[nx,ny]})
	Dcm -> Write, Stdy->DataPath(), imageM
	Dcm -> Trim, [min(imageM),0.8*max(imageM)]
	Dcm -> ReadDicom, files_TI[0]

	;Also calculate T1 map
	print, 'T1mapping'

	DynSeries = Stdy->Names(0,DefDim=3,ind=ind,sel=sel)
	ind = where(DynSeries EQ '*tfl2d1r86_Magnitude')
	T1Series = Stdy->Obj(0,ind)

	Dom = {z:T1Series->z(), t:T1Series->t(0), m:T1Series->m()}
    Sinf = Stdy->New('SERIES', Domain= Dom,  Name= T1Series->name() + '_Sinf' )
    Sratio = Stdy->New('SERIES', Domain= Dom,  Name= T1Series->name() + '_Sratio' )
    St1 = Stdy->New('SERIES', Domain= Dom,  Name= T1Series->name() + '_T1')
    St1corr = Stdy->New('SERIES', Domain= Dom,  Name= T1Series->name() + '_T1Corrected')

	d = T1Series->d()
	time = T1Series->t()
	ExpectedT1 = max(time)/4.0

	for j=0L,d[2]-1 do begin

		PMI__Message, status, 'Calculating ', j/(d[2]-1E)

		P = T1Series->Read(Stdy->DataPath(),z=T1Series->z(j))
		P = reform(P,d[0]*d[1],d[3],/overwrite)

		Sinf_slice = fltarr(d[0]*d[1])
		Sratio_slice = fltarr(d[0]*d[1])
		T1_slice = fltarr(d[0]*d[1])
		T1_corr_slice = fltarr(d[0]*d[1])

		for i=0L,d[0]*d[1]-1 do begin
			Sig = reform(P[i,*])
			; Identify the minimum and its index and reverse signs of all elements upto this index
			;minS = min(Sig,ind)
			;Sig[0:ind] = -Sig[0:ind]

			Pars = [max(Sig), 2.0, 1/ExpectedT1] ;[Sinf, Sratio(B/A), R1]
			Fit = mpcurvefit(time, Sig, 1+0E*Sig, Pars, function_name='PMI__TRISTAN_MOLLI_T1mapping',/quiet,NODERIVATIVE=1)

			Sinf_slice[i] = Pars[0]
			Sratio_slice[i] = Pars[1]
			T1_slice[i] = 1/Pars[2]
			T1_corr_slice[i] = (Pars[1]-1)/Pars[2]
		endfor

		Sinf->Write, Stdy->DataPath(), Sinf_slice, j
		Sratio->Write, Stdy->DataPath(), Sratio_slice, j
		St1->Write, Stdy->DataPath(), T1_slice, j
		St1corr->Write, Stdy->DataPath(), T1_corr_slice, j

	endfor

	Sinf->Trim, [0E, max(P)]
	Sratio->Trim, [0E, max(P)]
	St1->Trim, [0E, max(time)]
	St1corr->Trim, [0E, max(time)]

end


pro TRISTAN_Import_Siemens3T__LoadVolume, Sequence, Stdy, files, SeriesType, status

	PMI__Message, status, 'Loading ' + Sequence

	i = where(SeriesType eq Sequence, cnt)


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





pro TRISTAN_Import_Siemens3T__LoadSequence, Sequence, SortBy=SortBy, Stdy, files, first, status

  n = n_elements(first)-1

  ;Series = string(PMI__Dicom__Read(files[first[0:n-1]],'0018'x,'1030'x))
  ;Series = string(PMI__Dicom__Read(files[first[0]:first[n]-1],'0018'x,'0024'x))
  Series = string(PMI__Dicom__Read(files,'0018'x,'0024'x))
  i = where(Series eq Sequence, cnt)
  if cnt eq 0 then return
  Series = Series[i]
  files_SEQ = files[i]

  PMI__Message, status, 'Sorting ' + Sequence

  if n_elements(SortBy) eq 0 then SortBy = ['Slice location','Acquisition time']

  case SortBy[0] of
	'Slice location': sort_z = ['0020'x,'1041'x]
	'Image number': sort_z = ['0020'x,'0013'x]
  endcase

  case SortBy[1] of
	'Acquisition time': sort_t = ['0008'x,'0032'x]
	'Image number': sort_t = ['0020'x,'0013'x]
	'Echo time': sort_t = ['0018'x,'0081'x]  ;
  endcase

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


pro TRISTAN_Import_Siemens3T, Stdy, files, first, status=status

;;;;CHECK SEQUENCE PARAMETERS

;;;;LOAD THE SERIES

	seq1 = '*tfi2d1_192 ' ; All Localiser images bundled up

	seq2 = '*h2d1_208 _Cor ' ; Coronal T2 HASTE
	seq3 = '*h2d1_208 _Tra ' ; Transverse T2 HASTE

	seq4 = '*tfl2d1r86' ; LL T1 mapping

	seq5 = 'RAVE3d1 ' ; RAVE VFA and dynamic
	seq6 = '*fl3d1'  ; 3D SPGR - VFA and dynamic, BH and FB

	Series = string(PMI__Dicom__Read(files,'0018'x,'0024'x))


	Orientation = PMI__Dicom__Read(files,'0051'x,'100E'x)  ;Orientation of images
	SeriesType = Series + '_' + Orientation

	TRISTAN_Import_Siemens3T__LoadInversionRecovery, seq4, Stdy, files, Series, status
	TRISTAN_Import_Siemens3T__Load3DSPGR, seq6, Stdy, files, Series, status
	TRISTAN_Import_Siemens3T__LoadSequence, seq1, Stdy, files, first, status

	TRISTAN_Import_Siemens3T__LoadVolume, seq2, Stdy, files, SeriesType, status
	TRISTAN_Import_Siemens3T__LoadVolume, seq3, Stdy, files, SeriesType, status
	TRISTAN_Import_Siemens3T__LoadRAVE, seq5, Stdy, files, Series, status

end