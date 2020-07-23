

pro iBEAt_Import_Leeds_10_6__LoadIVIM, Stdy, Series, files, status

  Dcm = iBEAt_Import__LoadSequence(Stdy, Series, files, status)

;Define b-values & gradient vectors

  d = Dcm -> d()

  b = [$
  	0.0010000086, $
  	10.000086, $
  	19.99908294, $
  	30.00085926, $
  	50.00168544, $
  	80.0007135, $
  	100.0008375, $
  	199.9998135, $
  	300.0027313, $
  	600.0 ]

  bvalues = [b,b,b]

  gvector = fltarr(3,d[3]) ;Assumption - needs verification
  FOR i=0,9   DO gvector[*,i] = [1,0,0]
  FOR i=10,19 DO gvector[*,i] = [0,1,0]
  FOR i=20,29 DO gvector[*,i] = [0,0,1]

  Dcm -> set, obj_new('DATA_ELEMENT','0019'x,'100C'x,vr='FD',value=bvalues)
  Dcm -> set, obj_new('DATA_ELEMENT','0019'x,'100E'x,vr='FD',value=gvector)

end



pro iBEAt_Import_Leeds_10_6__LoadDTI, Stdy, Series, files, status

  Dcm = iBEAt_Import__LoadSequence(Stdy, Series, files, status, SORTED_FILES=files_sort)

;Read b-values & gradient vectors

  d = Dcm -> d()
  bvalues = fltarr(d[3])
  gvector = fltarr(3,d[3])

  for k=0L,d[3]-1 do begin

	PMI__Message, status, 'Reading b-matrix ' + Series, k/(d[3]-1.0)

    bvalues[k] = PMI__Dicom__Read(files_sort[0,k],'0019'x,'100C'x)
  	gvector[*,k] = PMI__Dicom__Read(files_sort[0,k],'0019'x,'100E'x)

  endfor

  Dcm -> set, obj_new('DATA_ELEMENT','0019'x,'100C'x,vr='FD',value=bvalues)
  Dcm -> set, obj_new('DATA_ELEMENT','0019'x,'100E'x,vr='FD',value=gvector)

end





FUNCTION iBEAt_Import_Leeds_10_6__SequenceName, file, number_of_images

  SeqName = PMI__Dicom__Read(file,'0018'x,'0024'x)

  IF SeqName EQ '*tfi2d1_192 ' THEN BEGIN
    FlipAngle = PMI__Dicom__Read(file,'0018'x,'1314'x)
    IF FlipAngle EQ 31.0 THEN RETURN, 'localizer_bh_fix'
    IF FlipAngle EQ 26.0 THEN RETURN, 'localizer_bh_ISO'
  ENDIF

  IF SeqName EQ '*h2d1_320 ' THEN BEGIN
    RETURN, 'T2w_abdomen_haste_tra_mbh'
  ENDIF

  IF SeqName EQ '*fl3d2' THEN BEGIN
    Type = PMI__Dicom__Read(file,'0008'x,'0008'x)
    IF type EQ 'ORIGINAL\PRIMARY\M\OUT_PHASE\NORM\DIS2D ' THEN RETURN, 'T1w_abdomen_dixon_cor_bh_out_phase'
    IF type EQ 'ORIGINAL\PRIMARY\M\IN_PHASE\NORM\DIS2D' THEN RETURN, 'T1w_abdomen_dixon_cor_bh_in_phase'
    IF type EQ 'ORIGINAL\PRIMARY\M\FAT\NORM\DIS2D ' THEN RETURN, 'T1w_abdomen_dixon_cor_bh_fat'
    IF type EQ 'ORIGINAL\PRIMARY\M\WATER\NORM\DIS2D ' THEN RETURN, 'T1w_abdomen_dixon_cor_bh_water'
  ENDIF

  IF SeqName EQ '*fl2d1r4' THEN BEGIN
    ImPos = PMI__Dicom__Read(file,'0020'x,'0032'x)
    IF ImPos[0] LE 0E THEN RETURN, 'PC_RenalArtery_Right_EcgTrig_fb_120'
    IF ImPos[0] GT 0E THEN RETURN, 'PC_RenalArtery_Left_EcgTrig_fb_120'
  ENDIF

  IF SeqName EQ '*fl2d1_v120in ' THEN BEGIN
    ImPos = PMI__Dicom__Read(file,'0020'x,'0032'x)
    Type = PMI__Dicom__Read(file,'0008'x,'0008'x)
    IF ImPos[0] LE 0E THEN BEGIN
      IF type EQ 'ORIGINAL\PRIMARY\MAG\RETRO\DIS2D' THEN RETURN, 'PC_RenalArtery_Right_EcgTrig_fb_120_magnitude'
  	  IF type EQ 'DERIVED\PRIMARY\P\RETRO\DIS2D ' THEN RETURN, 'PC_RenalArtery_Right_EcgTrig_fb_120_phase'
  	ENDIF
    IF ImPos[0] GT 0E THEN BEGIN
      IF type EQ 'ORIGINAL\PRIMARY\MAG\RETRO\DIS2D' THEN RETURN, 'PC_RenalArtery_Left_EcgTrig_fb_120_magnitude'
  	  IF type EQ 'DERIVED\PRIMARY\P\RETRO\DIS2D ' THEN RETURN, 'PC_RenalArtery_Left_EcgTrig_fb_120_phase'
  	ENDIF
  ENDIF

  IF SeqName EQ '*fl2d12 ' THEN BEGIN
    PhaseEncoding = PMI__Dicom__Read(file,'0018'x,'1312'x)
    Type = PMI__Dicom__Read(file,'0008'x,'0008'x)
    IF PhaseEncoding EQ 'COL ' THEN BEGIN
      IF type EQ 'ORIGINAL\PRIMARY\M\DIS2D' THEN RETURN, 'T2star_map_pancreas_tra_mbh_magnitude'
      IF type EQ 'ORIGINAL\PRIMARY\P\DIS2D' THEN RETURN, 'T2star_map_pancreas_tra_mbh_phase'
  	  IF type EQ 'DERIVED\PRIMARY\T2_STAR MAP\DIS2D ' THEN RETURN, 'T2star_map_pancreas_tra_mbh_T2star'
    ENDIF
    IF PhaseEncoding EQ 'ROW ' THEN BEGIN
      IF type EQ 'ORIGINAL\PRIMARY\M\DIS2D' THEN RETURN, 'T2map_kidneys_cor-oblique_mbh_magnitude'
      IF type EQ 'ORIGINAL\PRIMARY\P\DIS2D' THEN RETURN, 'T2map_kidneys_cor-oblique_mbh_phase'
  	  IF type EQ 'DERIVED\PRIMARY\T2_STAR MAP\DIS2D ' THEN RETURN, 'T2map_kidneys_cor-oblique_mbh_T2star'
    ENDIF
  ENDIF

  IF SeqName EQ '*fl2d1' THEN BEGIN
    Type = PMI__Dicom__Read(file,'0008'x,'0008'x)
    IF Type EQ 'ORIGINAL\PRIMARY\M\NORM\DIS2D ' THEN RETURN, 'T1w_kidneys_cor-oblique_mbh_magnitude'
    IF Type EQ 'ORIGINAL\PRIMARY\P\DIS2D' THEN RETURN, 'T1w_kidneys_cor-oblique_mbh_phase'
  ENDIF

  IF SeqName EQ '*tfl2d1r106 ' THEN BEGIN
    Type = PMI__Dicom__Read(file,'0008'x,'0008'x)
    IF Type EQ 'ORIGINAL\PRIMARY\M\DIS2D' THEN RETURN, 'T1map_kidneys_cor-oblique_mbh_magnitude'
    IF Type EQ 'ORIGINAL\PRIMARY\P\DIS2D' THEN RETURN, 'T1map_kidneys_cor-oblique_mbh_phase'
    IF (Type EQ 'ORIGINAL\PRIMARY\P\MOCO\DIS2D ') $ ;MoCo phase and magnitude interleaved in same series
    OR (Type EQ 'ORIGINAL\PRIMARY\M\MOCO\DIS2D ') THEN RETURN, 'T1map_kidneys_cor-oblique_mbh_moco'
    IF Type EQ 'DERIVED\PRIMARY\T1 MAP\MOCO\T1\DIS2D' THEN RETURN, 'T1map_kidneys_cor-oblique_mbh_T1map'
  ENDIF

  IF SeqName EQ '*tfl2d1r96' THEN BEGIN
    Type = PMI__Dicom__Read(file,'0008'x,'0008'x)
    IF Type EQ 'ORIGINAL\PRIMARY\M\DIS2D' THEN RETURN, 'T2map_kidneys_cor-oblique_mbh_magnitude'
    IF Type EQ 'ORIGINAL\PRIMARY\P\DIS2D' THEN RETURN, 'T2map_kidneys_cor-oblique_mbh_phase'
    IF Type EQ 'ORIGINAL\PRIMARY\M\DIS2D\MOCO ' THEN RETURN, 'T2map_kidneys_cor-oblique_mbh_magnitude_moco'
    IF Type EQ 'DERIVED\PRIMARY\T2 MAP\DIS2D\MOCO\T2' THEN RETURN, 'T2map_kidneys_cor-oblique_mbh_T2map'
  ENDIF

  IF STRMID(SeqName,0,5) EQ '*ep_b' THEN BEGIN
    IF number_of_images EQ 900 THEN RETURN, 'IVIM_kidneys_cor-oblique_fb'
    IF number_of_images EQ 4380 THEN RETURN, 'DTI_kidneys_cor-oblique_fb'
  ENDIF

  IF SeqName EQ '*fl3d1' THEN BEGIN
    ScanOptions = PMI__Dicom__Read(file,'0018'x,'0022'x)
    IF ScanOptions EQ 'PFP ' THEN RETURN, 'MT_OFF_kidneys_cor-oblique_bh'
    IF ScanOptions EQ 'PFP\MT' THEN RETURN, 'MT_ON_kidneys_cor-oblique_bh'
  ENDIF

  IF SeqName EQ '*tfi2d1_154 ' THEN BEGIN
    RETURN, 'ASL_planning_bh'
  ENDIF

  IF SeqName EQ 'tgse3d1_512 ' THEN BEGIN
    RETURN, 'ASL_kidneys_pCASL_cor-oblique_fb'
  ENDIF

  IF SeqName EQ '*tfl2d1_16' THEN BEGIN
    RETURN, 'DCE_kidneys_cor-oblique_fb'
  ENDIF

  IF SeqName EQ 'RAVE3d1 ' THEN BEGIN
    RETURN, 'RAVE_kidneys_fb'
  ENDIF

  RETURN, 'Sequence not recognized'

END





PRO iBEAt_Import_Leeds_10_6__UpdateSequenceNames, SeriesName

    nSeries = n_elements(SeriesName)

;Label post-contrast DIXONs

    injection = WHERE(SeriesName EQ 'DCE_kidneys_cor-oblique_fb', cnt)
    IF cnt GT 0 THEN BEGIN
      FOR j=injection[0]+1, nSeries-1 DO BEGIN
        IF STRMID(SeriesName[j],0,17) EQ 'T1w_abdomen_dixon' $
        THEN SeriesName[j] += '_post_contrast'
      ENDFOR
    ENDIF

;Label ASL derived images

   asl = WHERE(SeriesName EQ 'ASL_kidneys_pCASL_cor-oblique_fb', cnt)
   nr_of_asl_series = cnt/5E
   IF nr_of_asl_series GT 0 THEN BEGIN
     FOR i=0L,nr_of_asl_series-1 DO BEGIN
       SeriesName[asl[5*i+0]] += '_M0_moco'
       SeriesName[asl[5*i+1]] += '_PW_moco'
       SeriesName[asl[5*i+2]] += '_RBF_moco'
       SeriesName[asl[5*i+3]] += '_control_moco'
       SeriesName[asl[5*i+4]] += '_label0_moco'
     ENDFOR
   ENDIF

END





PRO iBEAt_Import_Leeds_10_6, Stdy, files, status=status

;Sort by series number

	n = n_elements(files)
	series_number = lonarr(n)
	for i=0L,n-1 do begin
		PMI__Message, status, 'Sorting by series number ', i/(n-1E)
		series_number[i] = PMI__Dicom__Read(files[i],'0020'x,'0011'x)
	endfor
	ind = sort(series_number)
	series_number = series_number[ind]
	files = files[ind]

;Get series names

	nSeries = n_elements(reduce(series_number,first))
	first = [first,n]
    SeriesName = STRARR(nSeries)
	FOR i=0L, nSeries-1 DO BEGIN
	  SeriesFiles = files[first[i]:first[i+1]-1]
	  SeriesName[i] = iBEAt_Import_Leeds_10_6__SequenceName(SeriesFiles[0],n_elements(SeriesFiles))
	ENDFOR
	iBEAt_Import_Leeds_10_6__UpdateSequenceNames, SeriesName

;Load series & save

    FOR i=0L, nSeries-1 DO BEGIN
      SeriesFiles = files[first[i]:first[i+1]-1]
      nr = series_number[first[i]]
  	  Name = '[' + strcompress(nr,/remove_all) +']_' + SeriesName[i]
  	  CASE SeriesName[i] OF
  	    'IVIM_kidneys_cor-oblique_fb': $
  	      iBEAt_Import_Leeds_10_6__LoadIVIM, Stdy, Name, SeriesFiles, status
  	    'DTI_kidneys_cor-oblique_fb': $
  	      iBEAt_Import_Leeds_10_6__LoadDTI, Stdy, Name, SeriesFiles, status
	    ELSE: $
	      Dcm = iBEAt_Import__LoadSequence(Stdy, Name, SeriesFiles, status)
	  ENDCASE
    ENDFOR

    Stdy -> SaveStdy

END