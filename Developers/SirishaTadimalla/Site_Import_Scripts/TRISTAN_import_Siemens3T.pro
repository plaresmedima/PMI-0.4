
pro TRISTAN_Import_Siemens3T__LoadVFA, Sequence, Stdy, files, Series, status

	sort_z = ['0020'x,'1041'x]
	sort_t = ['0008'x,'0032'x]
	z = PMI__Dicom__Read(files,sort_z[0],sort_z[1])
  	t = PMI__Dicom__Read(files,sort_t[0],sort_t[1])
  	files = PMI__Dicom__Sort(files, z, t)

  	nx = PMI__Dicom__Read(files,'0028'x,'0011'x)
  	ny = PMI__Dicom__Read(files,'0028'x,'0010'x)

	FA = PMI__Dicom__Read(files[0],'0018'x,'1314'x)

  	d = [max(nx),max(ny),n_elements(z),n_elements(t)]
  	Dcm = Stdy -> New('SERIES',	Name = Sequence+'_'+STRING(ROUND(FA)), Domain = {z:z, t:t, m:d[0:1]})

  	x = (d[0]-nx)/2
  	y = (d[1]-ny)/2

  	im = fltarr(d[0],d[1])
  	range = [0E,0E]
  	for k=0L,d[2]*d[3]-1 do begin

		PMI__Message, status, 'Loading ' + Sequence, k/(d[2]*d[3]-1.0)

		image = PMI__Dicom__ReadImage(files[k])
		if size(image,/n_dimensions) ne 0 then begin
			im[x[k]:x[k]+nx[k]-1,y[k]:y[k]+ny[k]-1] = image
			range[0] = min([range[0],min(image,max=max)])
			range[1] = max([range[1],max])
		endif
		Dcm -> Write, Stdy->DataPath(), im, k
		im = im*0

 	endfor

  	Dcm -> Trim, [range[0],0.8*range[1]]
  	Dcm -> ReadDicom, files[0]

end

pro TRISTAN_Import_Siemens3T__LoadDynamics, Sequence, Stdy, files, Series, status

	z = PMI__Dicom__Read(files,'0020'x,'1041'x) ;Array of Slice locations
	t = PMI__Dicom__Read(files,'0008'x,'0032'x) ;Array of Acquisition times
	files = PMI__Dicom__Sort(files, z, t) ; returns a 2D string array of file names

	nx = PMI__Dicom__Read(files[0],'0028'x,'0011'x)  ;nr of rows
	ny = PMI__Dicom__Read(files[0],'0028'x,'0010'x)  ;nr of columns
	nz = n_elements(z)
	nt = n_elements(t)

	d = [nx,ny,nz,nt] ;dimensions of the series
	Dcm = Stdy -> New('SERIES', Name=Sequence+ '_dynamic', Domain = {z:z, t:t, m:[nx,ny]}) ;new series object in display
	image = fltarr(nx,ny,nz*nt) ;array holding the data

	for k=0L,d[2]*d[3]-1 do begin ;loop over all slices and times

	  PMI__Message, status, 'Loading ' + Sequence , k/(d[2]*d[3]-1.0)
	  image[*,*,k] = PMI__Dicom__ReadImage(files[k]) ;read image data from file

	endfor

	Dcm -> Write, Stdy->DataPath(), image  ;write raw data to disk
	Dcm -> Trim, [min(image),0.8*max(image)]  ;set default grey scale ranges
	Dcm -> ReadDicom, files[0]  ;read DICOM header information for the series

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

	; Process BH dataset
	seriesNumbers = PMI__Dicom__Read(files_BH,'0020'x,'0011'x)
	uniqueSeries = seriesNumbers[UNIQ(seriesNumbers, SORT(seriesNumbers))]

	for n=0L,n_elements(uniqueSeries)-2 do begin
		j = where(PMI__Dicom__Read(files_BH,'0020'x,'0011'x) eq uniqueSeries[n])
		files_SEQ = files_BH[j]
		TRISTAN_Import_Siemens3T__LoadVFA, Sequence+'_BH', Stdy, files_SEQ, Series, status
	endfor

	j = where(PMI__Dicom__Read(files_BH,'0020'x,'0011'x) eq uniqueSeries[n_elements(uniqueSeries)-1])
	files_SEQ = files_BH[j]
	TRISTAN_Import_Siemens3T__LoadDynamics, Sequence+'_BH', Stdy, files_SEQ, Series, status

	; Process FB dataset
	seriesNumbers = PMI__Dicom__Read(files_FB,'0020'x,'0011'x)
	uniqueSeries = seriesNumbers[UNIQ(seriesNumbers, SORT(seriesNumbers))]

	for n=0L,n_elements(uniqueSeries)-1 do begin
		j = where(PMI__Dicom__Read(files_FB,'0020'x,'0011'x) eq uniqueSeries[n])
		files_SEQ = files_FB[j]
		TRISTAN_Import_Siemens3T__LoadDynamics, Sequence+'_FB', Stdy, files_SEQ, Series, status
	endfor

end

pro TRISTAN_Import_Siemens3T__LoadRAVE, Sequence, Stdy, files, Series, status

	PMI__Message, status, 'Loading RAVE Data'

	i = where(Series eq Sequence, cnt)
	if cnt eq 0 then return
	files_RAVE = files[i]

	; Separate individual flip angle and dynamic series using the series numbers
	seriesNumbers = PMI__Dicom__Read(files_RAVE,'0020'x,'0011'x)
	uniqueSeries = seriesNumbers[UNIQ(seriesNumbers, SORT(seriesNumbers))]

	; Get VFA datasets
	for n=0L,n_elements(uniqueSeries)-2 do begin
		j = where(PMI__Dicom__Read(files_RAVE,'0020'x,'0011'x) eq uniqueSeries[n])
		files_SEQ = files_RAVE[j]

	  	TRISTAN_Import_Siemens3T__LoadVFA, Sequence, Stdy, files_SEQ, Series, status

	endfor

	; Get dynamic dataset
	j = where(PMI__Dicom__Read(files_RAVE,'0020'x,'0011'x) eq uniqueSeries[n_elements(uniqueSeries)-1])
	files_SEQ = files_RAVE[j]

	TRISTAN_Import_Siemens3T__LoadDynamics, Sequence, Stdy, files_SEQ, Series, status


end

pro TRISTAN_Import_Siemens3T__LoadInversionRecovery, Sequence, Stdy, files, Series, status

	PMI__Message, status, 'Loading Inversion Recovery Data'

	i = where(Series eq Sequence, cnt)
	if cnt eq 0 then return
	files_IR = files[i]

	; Get only magnitude images
	j = where(PMI__Dicom__Read(files_IR,'0008'x,'0008'x) eq 'ORIGINAL\PRIMARY\M\DIS2D')

	files_M = files_IR[j]

	z = PMI__Dicom__Read(files_M,'0020'x,'1041'x) ;Slice location
	t = PMI__Dicom__Read(files_M,'2001'x,'101B'x) ;Inversion Time

	files_TI = PMI__Dicom__Sort(files_M, z, t)

	nx = PMI__Dicom__Read(files_TI[0],'0028'x,'0011'x)
	ny = PMI__Dicom__Read(files_TI[0],'0028'x,'0010'x)
	nz = n_elements(z)
	nt = n_elements(t)
	image = fltarr(nx,ny,nz*nt)

	for k=0L,nz*nt-1 do image[*,*,k] = PMI__Dicom__ReadImage(files_TI[k])

	Dcm = Stdy -> New('SERIES', Name=Sequence+'_Magnitude', Domain = {z:z, t:t, m:[nx,ny]})
	Dcm -> Write, Stdy->DataPath(), image
	Dcm -> Trim, [min(image),0.8*max(image)]
	Dcm -> ReadDicom, files_TI[0]

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

	TRISTAN_Import_Siemens3T__LoadSequence, seq1, Stdy, files, first, status

	TRISTAN_Import_Siemens3T__LoadVolume, seq2, Stdy, files, SeriesType, status
	TRISTAN_Import_Siemens3T__LoadVolume, seq3, Stdy, files, SeriesType, status

	TRISTAN_Import_Siemens3T__LoadInversionRecovery, seq4, Stdy, files, Series, status

	TRISTAN_Import_Siemens3T__LoadRAVE, seq5, Stdy, files, Series, status
	TRISTAN_Import_Siemens3T__Load3DSPGR, seq6, Stdy, files, Series, status

end