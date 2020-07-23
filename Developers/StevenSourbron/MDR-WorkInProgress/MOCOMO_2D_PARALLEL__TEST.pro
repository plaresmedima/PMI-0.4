
PRO MOCOMO_2D_PARALLEL__TEST, SETTINGS=settings

;SOURCE DATA

  DataPath = 'C:\Users\steve\Dropbox\Software\TestData\'
  AIF_file = DataPath + 'MRR_AIF.txt'
  MRR_file = DataPath + 'MRR_[384,384,1,265].dat'
  ns = [384,384,265]
  type = 4
  window_slice = {p:[0,0L], n:ns[0:1]}
  window_large = {p:[212,116L], n:[116,149L]} ;2.4min
  window_small = {p:[222,221L], n:[88,34L]} ;0.83min

;LOAD SOURCE DATA

  PMI__ReadPlot, AIF_file, time, aif
  Aif = LMU__Enhancement(Aif,nb,relative=0)/(1-0.45)
  Source = PMI__ReadData(MRR_file,type)
  Source = REFORM(Source, ns, /OVERWRITE)
  Source = TRANSPOSE(Source, [2,0,1])
  ns = [ns[2],ns[0],ns[1]]

;EXPORT SOURCE GIF

  IF 0 THEN BEGIN
    image = BYTE(255*(Source-min(Source))/(0.5*max(Source)-min(Source)))
    File = DataPath + 'Source.gif'
    LOADCT, 0
    FOR k=0L, ns[0]-1 DO write_gif, File, REFORM(image[k,*,*]), /MULTIPLE
    write_gif, File, /CLOSE
  ENDIF

;MOCOMO SETTINGS

  IF N_ELEMENTS(settings) EQ 0 THEN settings = 1 ;(1=development, 2=10min validatiom, 3=3hr validation)
  CASE settings OF
    1:BEGIN
      resolution = 64
      precision = 2.0
      w = window_small
      filename = 'upperpole'
      END
    2:BEGIN
      resolution = 64
      precision = 1.0
      w = window_large
      filename = 'leftkidney'
    END
    3:BEGIN
      resolution = 16
      precision = 1.0
      w = window_slice
      filename = 'wholeslice'
    END
  ENDCASE
  Modelname = 'TwoCompartmentFiltration'
  Independent = {t:time, ca:aif, n0:10}

;OPTIMIZE

  Model = OBJ_NEW('MoCoModel_' + ModelName, Independent)
  Model -> FIT, Source, Target
  start_ChiSq = total((Source - Target)^2.)
  start_time = systime(1)

  MOCOMO_2D_PARALLEL, Source, ModelName, Independent, GRID_SIZE=Resolution, TOLERANCE=Precision, WINDOW=w

;PERFORMANCE

  calculation_time = systime(1)-start_time
  Model -> FIT, Source, Target
  ChiSq = total((Source - Target)^2.)
  OBJ_DESTROY, Model

  time = strcompress(calculation_time/60.)
  improvement = strcompress(100*(start_ChiSq-ChiSq)/start_ChiSq)

  print, 'Parallel'
  print, 'Calculation time (min): ' + time
  print, 'Chi-Square reduction (%): '+ improvement

;EXPORT DEFORMED GIF

  image = BYTE(255*(Source-min(Source))/(0.5*max(Source)-min(Source)))
  File = DataPath + filename + '_parallel_[' + time + ' min, ' + improvement + ' %]' + '.gif'
  LOADCT, 0
  FOR k=0L,ns[0]-1 DO write_gif, File, REFORM(image[k,*,*]), /MULTIPLE
  write_gif, File, /CLOSE


END






