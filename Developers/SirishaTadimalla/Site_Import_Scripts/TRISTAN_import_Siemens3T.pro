
pro TRISTAN_Import_Siemens3T__LoadDynamics_MoCo, Sequence, Stdy, files, Series, status

	time = Series->t() - Series->t(0)
	Source = Series->Read(Stdy->DataPath())

	; Set parameters for MoCoMo
	in.res = 1E
	in.prec = 1E
	; Set window
	win = {p:[0L,0L,0L], n:d[0:2]}

	; Calculate
	Source = TRANSPOSE(Source, [3,0,1,2])
	Source = MOCOMO_3D(Source, 'QIM_CONST', 0B, in.res, in.prec, Win=win)
;	Source = MOCOMO_3D(Source, 'VFA', {TR:3.0, FA:[2,5,10,15,20,25]}, in.res, in.prec, Win=win)
    Source = TRANSPOSE(Source, [1,2,3,0])
    Dom = {z:Series->z(), t:Series->t(), m:Series->m()}
    Corr = Stdy->New('SERIES', Domain=Dom,  Name=Series->name() + '[Motion-free]' )
	Corr -> Write, Stdy->DataPath(), Source
	Corr -> Trim, Series->Trim()

end

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

	FA = PMI__Dicom__Read(files[0],'0018'x,'1314'x)

	d = [nx,ny,nz,nt] ;dimensions of the series
	Dcm = Stdy -> New('SERIES', Name=Sequence+ '_dynamic_'+STRING(ROUND(FA)), Domain = {z:z, t:t, m:[nx,ny]}) ;new series object in display
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
	print, n_elements(files)

	files_all = files[i]

	; Separate the breath-hold and free-breathing series using number of rows
	jBH = where(PMI__Dicom__Read(files_all,'0028'x,'0010'x) eq 256)
	jFB = where(PMI__Dicom__Read(files_all,'0028'x,'0010'x) eq 96)
	;print, PMI__Dicom__Read(files_all[0],'0028'x,'0010'x)

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
		TRISTAN_Import_Siemens3T__LoadDynamics_MoCo, Sequence+'_FB', Stdy, files_SEQ, Series, status
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

	; Get T1maps calculated by the scanner
	j = where(PMI__Dicom__Read(files_IR,'0008'x,'0008'x) eq 'DERIVED\PRIMARY\T1 MAP\MOCO\T1\DIS2D')
	files_map = files_IR[j]

	z = PMI__Dicom__Read(files_map,'0020'x,'1041'x) ;Slice location
	t = PMI__Dicom__Read(files_map,'0008'x,'0032'x) ;Array of Acquisition times

	files_T1 = PMI__Dicom__Sort(files_map, z, t) ; returns a 2D string array of file names
	nx = PMI__Dicom__Read(files_T1[0],'0028'x,'0011'x)
	ny = PMI__Dicom__Read(files_T1[0],'0028'x,'0010'x)
	nz = n_elements(z)

	image = fltarr(nx,ny,nz)

	for k=0L,nz-1 do image[*,*,k] = PMI__Dicom__ReadImage(files_T1[k])


	Dcm = Stdy -> New('SERIES', Name=Sequence+'_SiemensT1map (ms)', Domain = {z:z, t:t, m:[nx,ny]})
	Dcm -> Write, Stdy->DataPath(), image
	Dcm -> Trim, [min(image),0.8*max(image)]
	Dcm -> ReadDicom, files[0]

;	;Also calculate T1 map
;	print, 'T1mapping'
;
;	R1map = fltarr(nx,ny,nz)
;
;	for j=0L,0L do begin ; loop over all slices ;nz-1
;
;		PMI__Message, status, 'Calculating ', j/(nz-1E)
;
;		; iterative fitting using the shifting polarity method
;		; for the expected range of T1 between 100 and 2000 ms and the range of inversion times in this dataset,
;		; only the first 10 time points need to be reversed
;		tmp1 = rebin(lindgen(n_elements(t)),n_elements(t),n_elements(t))
;		tmp2 = rebin(transpose(lindgen(n_elements(t))),n_elements(t),n_elements(t))
;		mask1 = (tmp1 GT tmp2)
;		mask2 = (tmp1 LE tmp2)
;		polarity = LONG(mask1)-LONG(mask2)
;		ExpectedT1 = max(t)/4.0
;
;		for i=58700L,58710 do begin ;nx*ny-1
;
;			Sig = imageM[i,nt*j:nt*(j+1)-1]
;			gof = fltarr(n_elements(t))
;
;			for k=0L,n_elements(t)-1 do begin
;				S = Sig*polarity[*,k]
;				Pars = [max(S), 2.0*max(S), 1.0/ExpectedT1]
;				Fit = mpcurvefit(t, S, 1+0E*S, Pars, function_name='FITMOLLIT1',/quiet,NODERIVATIVE=1)
;				gof[k] = total(abs(Fit-S))
;			endfor
;
;			if 	value_locate(gof, min(gof)) GE 0 then begin
;				ind = value_locate(gof, min(gof))
;				S = Sig*polarity[*,ind]
;				Pars = [max(S), 2.0*max(S), 1.0/ExpectedT1]
;				Fit = mpcurvefit(t, S , 1+0E*S, Pars, function_name='FITMOLLIT1',/quiet,NODERIVATIVE=1)
;				print, Pars
;				A = Pars[0]
;				B = Pars[1]
;				R1app = Pars[2]
;				R1map[*,*,j] = 1000.0*R1app*A/(B-A)
;			endif else begin
;				R1map[*,*,j] = 0
;			endelse
;
;		endfor
;
;	endfor
;	Dcm = Stdy -> New('SERIES', Name=Sequence+'_R1map (/s)', Domain = {z:z, t:t[0], m:[nx,ny]})
;	Dcm -> Write, Stdy->DataPath(), R1map
;	Dcm -> Trim, [min(R1map),0.8*max(R1map)]
;	Dcm -> ReadDicom, files_TI[0]
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

	;TRISTAN_Import_Siemens3T__LoadSequence, seq1, Stdy, files, first, status

	;TRISTAN_Import_Siemens3T__LoadVolume, seq2, Stdy, files, SeriesType, status
	;TRISTAN_Import_Siemens3T__LoadVolume, seq3, Stdy, files, SeriesType, status

	;TRISTAN_Import_Siemens3T__LoadInversionRecovery, seq4, Stdy, files, Series, status

	;TRISTAN_Import_Siemens3T__Load3DSPGR, seq6, Stdy, files, Series, status
	TRISTAN_Import_Siemens3T__LoadRAVE, seq5, Stdy, files, Series, status

end