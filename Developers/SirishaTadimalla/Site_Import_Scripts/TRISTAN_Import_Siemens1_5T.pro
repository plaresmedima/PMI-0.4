

pro iBEAt_Import_Turku_10_8__LoadT1W, Sequence, Stdy, files, Series, status

	PMI__Message, status, 'Sorting ' + Sequence

	i = where(Series eq Sequence, cnt)
	if cnt eq 0 then return
	files_T1W = files[i]

	z = PMI__Dicom__Read(files_T1W,'0020'x,'1041'x) ;Slice location
	t = PMI__Dicom__Read(files_T1W,'0020'x,'0013'x) ;Image Number
	files_T1W = PMI__Dicom__Sort(files_T1W, z, t)

	nx = PMI__Dicom__Read(files_T1W[0],'0028'x,'0011'x)
	ny = PMI__Dicom__Read(files_T1W[0],'0028'x,'0010'x)

	Type = ['Magnitude', 'Phase']
	d = [nx,ny,n_elements(z),n_elements(t)]

	for ti=0L,d[3]-1 do begin

	  Dcm = Stdy -> New('SERIES', Name=Sequence+'_'+Type[ti], Domain = {z:z, t:t[ti], m:[nx,ny]})
	  range = [0E,0E]
	  for k=ti*d[2],(ti+1)*d[2]-1 do begin

        PMI__Message, status, 'Loading  ' + Sequence, k/(d[2]*d[3]-1.0)

		image = PMI__Dicom__ReadImage(files_T1W[k])
		range[0] = min([range[0],min(image,max=max)])
		range[1] = max([range[1],max])
		Dcm -> Write, Stdy->DataPath(), image, k-ti*d[2]

	  endfor

	  Dcm -> Trim, [range[0],0.8*range[1]]
	  Dcm -> ReadDicom, files_T1W[0]

	endfor

end








pro iBEAt_Import_Turku_10_8__LoadDixon, Sequence, Stdy, files, Series, status

	PMI__Message, status, 'Sorting ' + Sequence

	i = where(Series eq Sequence, cnt)
	if cnt eq 0 then return
	files_DIX = files[i]

	z = PMI__Dicom__Read(files_DIX,'0020'x,'1041'x) ;Slice location
	t = PMI__Dicom__Read(files_DIX,'0020'x,'0013'x) ;Image Number
	files_DIX = PMI__Dicom__Sort(files_DIX, z, t)

	nx = PMI__Dicom__Read(files_DIX[0],'0028'x,'0011'x)
	ny = PMI__Dicom__Read(files_DIX[0],'0028'x,'0010'x)

	Volumes = ['Water', 'In-phase', 'Op-phase', 'Fat']
	d = [nx,ny,n_elements(z),n_elements(t)]

	for ti=0L,d[3]-1 do begin

	  Dcm = Stdy -> New('SERIES', Name=Sequence+'_'+Volumes[ti], Domain = {z:z, t:t[ti], m:[nx,ny]})
	  range = [0E,0E]
	  for k=ti*d[2],(ti+1)*d[2]-1 do begin

		PMI__Message, status, 'Loading '+ Sequence, k/(d[2]*d[3]-1.0)

		image = PMI__Dicom__ReadImage(files_DIX[k])
		range[0] = min([range[0],min(image,max=max)])
		range[1] = max([range[1],max])
		Dcm -> Write, Stdy->DataPath(), image, k-ti*d[2]

	  endfor

	  Dcm -> Trim, [range[0],0.8*range[1]]
	  Dcm -> ReadDicom, files_DIX[0]

	endfor

end









pro iBEAt_Import_Turku_10_8__LoadMTmapping, Seq, Stdy, files, Series, status

	PMI__Message, status, 'Loading MT Data'

	Sequence = Seq[0]

	i = where(Series eq Sequence, cnt)
	if cnt eq 0 then return
	files_MT = files[i]

	z = PMI__Dicom__Read(files_MT,'0020'x,'1041'x) ;Slice location
	t = PMI__Dicom__Read(files_MT,'0020'x,'0013'x) ;Image Number
	files_MT = PMI__Dicom__Sort(files_MT, z, t)

	nx = PMI__Dicom__Read(files_MT[0],'0028'x,'0011'x)
	ny = PMI__Dicom__Read(files_MT[0],'0028'x,'0010'x)
	nz = n_elements(z)
	nt = n_elements(t)
	image_OFF = fltarr(nx,ny,nz,nt)

	for k=0L,nz-1 do begin
	for l=0L,nt-1 do begin
	  image_OFF[*,*,k,l] = PMI__Dicom__ReadImage(files_MT[k,l])
	endfor
	endfor

	Sequence = Seq[1]

	i = where(Series eq Sequence, cnt)
	if cnt eq 0 then return
	files_MT = files[i]

	z = PMI__Dicom__Read(files_MT,'0020'x,'1041'x) ;Slice location
	t = PMI__Dicom__Read(files_MT,'0020'x,'0013'x) ;Image Number
	files_MT = PMI__Dicom__Sort(files_MT, z, t)

	nx = PMI__Dicom__Read(files_MT[0],'0028'x,'0011'x)
	ny = PMI__Dicom__Read(files_MT[0],'0028'x,'0010'x)
	nz = n_elements(z)
	nt = n_elements(t)
	image_ON = fltarr(nx,ny,nz,nt)

	for k=0L,nz-1 do begin
	for l=0L,nt-1 do begin
	  image_ON[*,*,k,l] = PMI__Dicom__ReadImage(files_MT[k,l])
	endfor
	endfor

    Name = 'mt-kidney-coronal-oblique-bh_magnitude'
	Magnitude = fltarr(nx,ny,nz,2)
	Magnitude[*,*,*,0] = image_OFF[*,*,*,0]
	Magnitude[*,*,*,1] = image_ON[*,*,*,0]
	Dcm = Stdy -> New('SERIES', Name=Name, Domain = {z:z, t:[0,1], m:[nx,ny]})
	Dcm -> Write, Stdy->DataPath(), Magnitude
	Dcm -> Trim, [min(Magnitude),0.8*max(Magnitude)]
	Dcm -> ReadDicom, files_MT[0]

    Name = 'mt-kidney-coronal-oblique-bh_phase'
	Phase = fltarr(nx,ny,nz,2)
	Phase[*,*,*,0] = image_OFF[*,*,*,1]
	Phase[*,*,*,1] = image_ON[*,*,*,1]
	Dcm = Stdy -> New('SERIES', Name=Name, Domain = {z:z, t:[0,1], m:[nx,ny]})
	Dcm -> Write, Stdy->DataPath(), Phase
	Dcm -> Trim, [min(Phase),0.8*max(Phase)]
	Dcm -> ReadDicom, files_MT[0]

	Name = 'mt-kidney-coronal-oblique-bh_MTR'
	MTR = 100*(image_OFF[*,*,*,0]-image_ON[*,*,*,0])/image_OFF[*,*,*,0]
	Dcm = Stdy -> New('SERIES', Name=Name, Domain = {z:z, t:[0], m:[nx,ny]})
	Dcm -> Write, Stdy->DataPath(), MTR
	Dcm -> Trim, [0,100]
	Dcm -> ReadDicom, files_MT[0]

end








pro iBEAt_Import_Turku_10_8__LoadPhaseContrast, Sequence, Stdy, files, Series, status

	PMI__Message, status, 'Loading Phase Contrast Data'

	i = where(Series eq Sequence, cnt)
	if cnt eq 0 then return
	files_PC = files[i]

	z = PMI__Dicom__Read(files_PC,'0020'x,'1041'x) ;Slice location
	t = PMI__Dicom__Read(files_PC,'0020'x,'0013'x) ;Image Number
	files_PC = PMI__Dicom__Sort(files_PC, z, t)

	nx = PMI__Dicom__Read(files_PC[0],'0028'x,'0011'x)
	ny = PMI__Dicom__Read(files_PC[0],'0028'x,'0010'x)
	nz = n_elements(z)
	nt = n_elements(t)
	image = fltarr(nx,ny,nz,nt)

	for k=0L,nz-1 do begin
	for l=0L,nt-1 do begin
	  image[*,*,k,l] = PMI__Dicom__ReadImage(files_PC[k,l])
	endfor
	endfor

	nPhases = n_elements(files_PC)/3
	Trigger_Times = PMI__Dicom__Read(files_PC[0,2*nPhases:3*nPhases-1],'0018'x,'1060'x)

    Name = Sequence + '_Magnitude'
	Derived = Image[*,*,*,0:nPhases-1]
	Dcm = Stdy -> New('SERIES', Name=Name, Domain = {z:z, t:Trigger_Times, m:[nx,ny]})
	Dcm -> Write, Stdy->DataPath(), Derived
	Dcm -> Trim, [min(Derived),0.8*max(Derived)]
	Dcm -> ReadDicom, files_PC[0]

    Name = Sequence + '_Subtraction'
	Derived = Image[*,*,*,nPhases:2*nPhases-1]
	Dcm = Stdy -> New('SERIES', Name=Name, Domain = {z:z, t:Trigger_Times, m:[nx,ny]})
	Dcm -> Write, Stdy->DataPath(), Derived
	Dcm -> Trim, [min(Derived),0.8*max(Derived)]
	Dcm -> ReadDicom, files_PC[0]

    Name = Sequence + '_Velocity [cm/s]'
	Derived = Image[*,*,*,2*nPhases:3*nPhases-1]
	Dcm = Stdy -> New('SERIES', Name=Name, Domain = {z:z, t:Trigger_Times, m:[nx,ny]})
	Dcm -> Write, Stdy->DataPath(), Derived
	Dcm -> Trim, [min(Derived),max(Derived)]
	Dcm -> ReadDicom, files_PC[0]

end







pro iBEAt_Import_Turku_10_8__LoadT2mapping, Sequence, Stdy, files, Series, status

	PMI__Message, status, 'Loading T2 mapping Data'

    TI = [0.,30.,60.,70.,80.,90.,100.,110.,120.]

    s = 0L
	i = where(Series eq Sequence[s], cnt)
	if cnt eq 0 then return
	files_TI = files[i]
	nslices = n_elements(files_TI)
	t = fltarr(nslices) + TI[s]
	for s=1L, n_elements(Sequence)-1 do begin
		i = where(Series eq Sequence[s])
		files_TI = [files_TI, files[i]]
		t = [t, fltarr(nslices) + TI[s]]
	endfor

	z = PMI__Dicom__Read(files_TI,'0020'x,'1041'x) ;Slice location
	files_TI = PMI__Dicom__Sort(files_TI, z, t)

	nx = PMI__Dicom__Read(files_TI[0],'0028'x,'0011'x)
	ny = PMI__Dicom__Read(files_TI[0],'0028'x,'0010'x)
	nz = n_elements(z)
	nt = n_elements(t)
	image = fltarr(nx,ny,nz*nt)

	for k=0L,nz*nt-1 do image[*,*,k] = PMI__Dicom__ReadImage(files_TI[k])

	Name = 'T2map-kidney-coronal-oblique-BH-T1-TFE'
	Dcm = Stdy -> New('SERIES', Name=Name, Domain = {z:z, t:t, m:[nx,ny]})
	Dcm -> Write, Stdy->DataPath(), image
	Dcm -> Trim, [min(image),0.8*max(image)]
	Dcm -> ReadDicom, files_TI[0]

end






pro iBEAt_Import_Turku_10_8__LoadInversionRecovery, Sequence, Stdy, files, Series, status

	PMI__Message, status, 'Loading Inversion Recovery Data'

	i = where(Series eq Sequence[0], cnt)
	if cnt eq 0 then return
	files_TI = files[i]
	for s=1L, n_elements(Sequence)-1 do begin
		i = where(Series eq Sequence[s])
		files_TI = [files_TI, files[i]]
	endfor

	z = PMI__Dicom__Read(files_TI,'0020'x,'1041'x) ;Slice location
	t = PMI__Dicom__Read(files_TI,'2001'x,'101B'x) ;Inversion Time
	files_TI = PMI__Dicom__Sort(files_TI, z, t)

	nx = PMI__Dicom__Read(files_TI[0],'0028'x,'0011'x)
	ny = PMI__Dicom__Read(files_TI[0],'0028'x,'0010'x)
	nz = n_elements(z)
	nt = n_elements(t)
	image = fltarr(nx,ny,nz*nt)

	for k=0L,nz*nt-1 do image[*,*,k] = PMI__Dicom__ReadImage(files_TI[k])

	Name = 'T1map-kidney-BTFEcoronal-oblique-BH'
	Dcm = Stdy -> New('SERIES', Name=Name, Domain = {z:z, t:t, m:[nx,ny]})
	Dcm -> Write, Stdy->DataPath(), image
	Dcm -> Trim, [min(image),0.8*max(image)]
	Dcm -> ReadDicom, files_TI[0]

end






pro iBEAt_Import_Turku_10_8__LoadVolume, Sequence, Stdy, files, Series, status

	PMI__Message, status, 'Loading ' + Sequence

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





pro iBEAt_Import_Turku_10_8__LoadSequence, Sequence, SortBy=SortBy, Stdy, files, first, status

  n = n_elements(first)-1
  Series = string(PMI__Dicom__Read(files[first[0:n-1]],'0018'x,'1030'x))

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

  i = where(Series eq Sequence, cnt)
  for j=0L, cnt-1 do begin

	files_SEQ = files[first[i[j]]:first[i[j]+1]-1]

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

  endfor

end








pro iBEAt_Import_Turku_10_8, Stdy, files, first, status=status

;;;;CHECK SEQUENCE PARAMETERS


;;;;LOAD THE SERIES

	seq1 = 'WIP SURVEY-ISO-BH '
	seq2 = 'WIP SURVEY-REF'
	seq3 = 'WIP T2W-abdomen-haste-tra-bh SENSE'
	seq4 = 'WIP T1W-abdomen-Dixon-coronal-BH SENSE'
	seq5 = 'WIP T2star-map-pancreas-tra-bh SENSE'
	seq6 = 'WIP T1W-kidneys-coronal-oblique-bh SENSE'
	Seq7 =['WIP T1map-kidney-BTFE150-coronal-oblique-BH SENSE ', $
	  	   'WIP T1map-kidney-BTFE260coronal-oblique-BH SENSE', $
	       'WIP T1map-kidney-BTFE660coronal-oblique-BH SENSE', $
	       'WIP T1map-kidney-BTFE1055coronal-oblique-BH SENSE ', $
	       'WIP T1map-kidney-BTFE1535coronal-oblique-BH SENSE ', $
	       'WIP T1map-kidney-BTFE2012coronal-oblique-BH SENSE ', $
	       'WIP T1map-kidney-BTFE2573coronal-oblique-BH SENSE ', $
	       'WIP T1map-kidney-BTFE3450coronal-oblique-BH SENSE ', $
	       'WIP T1map-kidney-BTFE4405coronal-oblique-BH SENSE ']
	seq8 = 'WIP T2-star-map-kidneys-coronal-oblique-BH SENSE'
	seq9 = [$
	  'WIP T2map-kidney-coronal-oblique-BH-T1-TFE-no SENSE ', $
	  'WIP T2map-kidney-coronal-oblique-BH-T1-TFE-30 SENSE ', $
	  'WIP T2map-kidney-coronal-oblique-BH-T1-TFE-60 SENSE ', $
	  'WIP T2map-kidney-coronal-oblique-BH-T1-TFE-70 SENSE ', $
	  'WIP T2map-kidney-coronal-oblique-BH-T1-TFE-80 SENSE ', $
	  'WIP T2map-kidney-coronal-oblique-BH-T1-TFE-90 SENSE ', $
	  'WIP T2map-kidney-coronal-oblique-BH-T1-TFE-100 SENSE', $
	  'WIP T2map-kidney-coronal-oblique-BH-T1-TFE-110 SENSE', $
	  'WIP T2map-kidney-coronal-oblique-BH-T1-TFE-120 SENSE']
	seq10 =[$
	  'WIP mt-off-kidney-coronal-oblique-bh SENSE', $
	  'WIP mt-on-kidney-coronal-oblique-bh SENSE ']
	seq11 = 'WIP DTI-kidney-coronal-oblique-FB SENSE '
	seq12 = 'WIP ss-EPI-pCASL-TRAslap-FA40-FS SENSE'
	seq13 = 'WIP M0-pCASL-FA40 SENSE '
	seq14 = 'WIP SURVEY_PCA'
	seq15 = 'WIP PC-left-RT-ECGtrig-fb '
	seq16 = 'WIP PC-right-RT-ECGtrig-fb'
	seq17 = 'WIP DCE-kidney-coronal-oblique-FB-dry-run SENSE '
	seq18 = 'WIP DCE-kidney-coronal-oblique-FB SENSE '
	seq19 = 'WIP T1W-abdomen-Dixon-post-coronal-BH SENSE '


	Series = string(PMI__Dicom__Read(files,'0018'x,'1030'x))

	iBEAt_Import_Turku_10_8__LoadSequence, seq1, Sortby=['Image number','Acquisition time'], Stdy, files, first, status
	iBEAt_Import_Turku_10_8__LoadSequence, seq2, Sortby=['Image number','Acquisition time'], Stdy, files, first, status
	iBEAt_Import_Turku_10_8__LoadVolume, seq3, Stdy, files, Series, status
	iBEAt_Import_Turku_10_8__LoadDixon, seq4, Stdy, files, Series, status
	iBEAt_Import_Turku_10_8__LoadSequence, seq5, Sortby=['Slice location','Echo time'], Stdy, files, first, status
	iBEAt_Import_Turku_10_8__LoadT1W, seq6, Stdy, files, Series, status
	iBEAt_Import_Turku_10_8__LoadInversionRecovery, seq7, Stdy, files, Series, status
	iBEAt_Import_Turku_10_8__LoadSequence, seq8, Sortby=['Slice location','Echo time'], Stdy, files, first, status
	iBEAt_Import_Turku_10_8__LoadT2mapping, seq9, Stdy, files, series, status
	iBEAt_Import_Turku_10_8__LoadMTmapping, seq10, Stdy, files, series, status
	iBEAt_Import_Turku_10_8__LoadSequence, seq11, Stdy, files, first, status
	iBEAt_Import_Turku_10_8__LoadSequence, seq12, Stdy, files, first, status
	iBEAt_Import_Turku_10_8__LoadSequence, seq13, Stdy, files, first, status
	iBEAt_Import_Turku_10_8__LoadSequence, seq14, Stdy, files, first, status
	iBEAt_Import_Turku_10_8__LoadPhaseContrast, seq15, Stdy, files, series, status
	iBEAt_Import_Turku_10_8__LoadPhaseContrast, seq16, Stdy, files, series, status
	iBEAt_Import_Turku_10_8__LoadSequence, seq17, Stdy, files, first, status
	iBEAt_Import_Turku_10_8__LoadSequence, seq18, Stdy, files, first, status
	iBEAt_Import_Turku_10_8__LoadDixon, seq19, Stdy, files, Series, status

end