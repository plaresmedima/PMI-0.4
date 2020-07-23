
;FUNCTION MOCOMO_2D_GROUPWISE__TEST_NUMGGRAD, Source, deformation_field, model, parameters, xc, yc, location
;
;;Calculate the gradient for Groupwise MoCoMo numerically
;;Approach: Take a number of steps smaller than one pixel size
;;and extrapolate linearly to stepsize of zero
;
;  nstepsizes = 10
;  maxstepsize = 1.0 ;pixelsize
;  minstepsize = 0.2 ;pixelsize
;
;  deformed = GFFD_2D(Source, deformation_field, xc, yc)
;  ChiSq = Model->CHISQ(parameters, deformed)
;
;  k = location[0]
;  d = location[1]
;  x = location[2]
;  y = location[3]
;
;  deformation_field_k = REFORM(deformation_field[k,*,*,*])
;  source_k = REFORM(Source[k,*,*])
;  deformed_step = deformed
;
;  stepsize = minstepsize + (maxstepsize-minstepsize)*findgen(nstepsizes)/(nstepsizes-1.0)
;  numerical_gradient = fltarr(nstepsizes)
;
;  FOR i=0L,nstepsizes-1 DO BEGIN
;
;    deformation_field_k[d,x,y] += stepsize[i]
;  	deformed_step[k,*,*] = FFD_2D(source_k, deformation_field_k, xc, yc)
;  	ChiSq_step = Model->CHISQ(parameters, deformed_step)
;  	numerical_gradient[i] = (ChiSq - ChiSq_step)/stepsize[i]
;  	deformed_step[k,*,*] = deformed[k,*,*]
;  	deformation_field_k[d,x,y] -= stepsize[i]
;
;  ENDFOR
;
;  AB = LINFIT(stepsize, numerical_gradient)
;  plot, [0,stepsize], AB[0]+AB[1]*[0,stepsize]
;  oplot, stepsize, numerical_gradient, psym=4
;
;  RETURN, AB[0]
;
;END
;
;
;
;
;
;PRO MOCOMO_2D_GROUPWISE__TEST_BRUTELSEARCH, Grad, Source, deformed, deformation_field, model, parameters, xc, yc
;
;  nstepsizes = 100
;  maxstepsize = 0.1 ;pixelsize
;  minstepsize = 0.0 ;pixelsize
;
;  stepsize = minstepsize + (maxstepsize-minstepsize)*findgen(nstepsizes)/(nstepsizes-1.0)
;  ChiSq = fltarr(nstepsizes)
;
;  Grad /= sqrt(max(total(Grad^2,2)))
;
;  FOR k=0L, nstepsizes-1 DO BEGIN
;
;	F_try = deformation_field + stepsize[k]*Grad
;	D_try = GFFD_2D(Source, F_try, xc, yc)
;	ChiSq[k] = Model->CHISQ(parameters, D_try)
;
;  ENDFOR
;
;  minChiSq = min(chisq, ind)
;  print, 'Stepsize (brute force): ', stepsize[ind]
;
;  plot, stepsize, chisq, /ystyle
;  oplot, [stepsize[ind]], [chisq[ind]], psym=4
;
;END
;
;
;
;
;
;PRO MOCOMO_2D_GROUPWISE__TEST_LSEARCH
;
;;SOURCE DATA
;
;  DataPath = 'C:\Users\steve\Dropbox\Software\TestData\'
;  AIF_file = DataPath + 'MRR_AIF.txt'
;  MRR_file = DataPath + 'MRR_[384,384,1,265].dat'
;  ns = [384,384,265]
;  type = 4
;  window_slice = {p:[0,0L], n:ns[0:1]}
;  window_large = {p:[212,116L], n:[116,149L]} ;2.4min
;  window_small = {p:[222,221L], n:[88,34L]} ;0.83min
;
;;LOAD SOURCE DATA
;
;  PMI__ReadPlot, AIF_file, time, aif
;  Aif = LMU__Enhancement(Aif,nb,relative=0)/(1-0.45)
;  Source = PMI__ReadData(MRR_file,type)
;  Source = REFORM(Source, ns, /OVERWRITE)
;  Source = TRANSPOSE(Source, [2,0,1])
;  ns = [ns[2],ns[0],ns[1]]
;
;;MOCOMO SETTINGS
;
;  resolution = 64
;  precision = 0.01
;  w = window_small
;  Modelname = 'TwoCompartmentFiltration'
;  Independent = {t:time, ca:aif, n0:10}
;
;;PERFORM FIRST LINE SEARCH
;
;  Model = OBJ_NEW('MoCoModel_' + ModelName, Independent)
;
;  Sx = Source[*,1:ns[1]-1,*] - Source[*,0:ns[1]-2,*] ;derivative of source image with respect to x
;  Sy = Source[*,*,1:ns[2]-1] - Source[*,*,0:ns[2]-2] ;derivative of source image with respect to y
;
;  xw = w.p[0] + lindgen(w.n[0])
;  yw = w.p[1] + lindgen(w.n[1])
;
;  multi_resolution_grid = MOCOMO_2D_GRID(w.n, Resolution)
;  nr_of_resolution_levels = n_elements(multi_resolution_grid[*,0])
;
;  Deformed = Source[*, xw, yw]
;
;  resolution_level = 0
;  grid_size = reform(multi_resolution_grid[resolution_level,*])
;  deformation_field = MOCOMO_2D_INIT(ns[0], w, grid_size)
;
;  FFD_2D_PRECOMPUTE, grid_size, w.n, xc, yc, Weight_cnt, Weight_loc, Weight_val
;  parameters = Model -> PARAMETERS(deformed)
;  stepsize = 1.0 ;initial stepsize in pixels
;  Grad = GFFD_2D_GRAD(Sx, Sy, deformed, deformation_field, model, parameters, xc, yc, Weight_cnt, Weight_loc, Weight_val)
;
;  MOCOMO_2D_UNITTEST_BRUTELSEARCH, Grad, Source, deformed, deformation_field, model, parameters, xc, yc
;  ok = GFFD_2D_LSEARCH(Grad, Source, deformed, deformation_field, model, parameters, xc, yc, precision, STEPSIZE=stepsize)
;  print, 'Stepsize (line search): ', stepsize
;
;  OBJ_DESTROY, Model
;
;END
;
;
;
;
;
;PRO MOCOMO_2D_GROUPWISE__TEST_GRADIENT
;
;;SOURCE DATA
;
;  DataPath = 'C:\Users\steve\Dropbox\Software\TestData\'
;  AIF_file = DataPath + 'MRR_AIF.txt'
;  MRR_file = DataPath + 'MRR_[384,384,1,265].dat'
;  ns = [384,384,265]
;  type = 4
;  window_slice = {p:[0,0L], n:ns[0:1]}
;  window_large = {p:[212,116L], n:[116,149L]} ;2.4min
;  window_small = {p:[222,221L], n:[88,34L]} ;0.83min
;
;;LOAD SOURCE DATA
;
;  PMI__ReadPlot, AIF_file, time, aif
;  Aif = LMU__Enhancement(Aif,nb,relative=0)/(1-0.45)
;  Source = PMI__ReadData(MRR_file,type)
;  Source = REFORM(Source, ns, /OVERWRITE)
;  Source = TRANSPOSE(Source, [2,0,1])
;  ns = [ns[2],ns[0],ns[1]]
;
;;MOCOMO SETTINGS
;
;  resolution = 64
;  precision = 0.01
;  w = window_small
;  Modelname = 'TwoCompartmentFiltration'
;  Independent = {t:time, ca:aif, n0:10}
;
;;CALCULATE FIRST GRADIENT
;
;  Model = OBJ_NEW('MoCoModel_' + ModelName, Independent)
;
;  Sx = Source[*,1:ns[1]-1,*] - Source[*,0:ns[1]-2,*] ;derivative of source image with respect to x
;  Sy = Source[*,*,1:ns[2]-1] - Source[*,*,0:ns[2]-2] ;derivative of source image with respect to y
;
;  xw = w.p[0] + lindgen(w.n[0])
;  yw = w.p[1] + lindgen(w.n[1])
;
;  multi_resolution_grid = MOCOMO_2D_GRID(w.n, Resolution)
;  nr_of_resolution_levels = n_elements(multi_resolution_grid[*,0])
;
;  Deformed = Source[*, xw, yw]
;
;  resolution_level = 0
;  grid_size = reform(multi_resolution_grid[resolution_level,*])
;  deformation_field = MOCOMO_2D_INIT(ns[0], w, grid_size)
;
;  FFD_2D_PRECOMPUTE, grid_size, w.n, xc, yc, Weight_cnt, Weight_loc, Weight_val
;  parameters = Model -> PARAMETERS(deformed)
;  Grad = GFFD_2D_GRAD(Sx, Sy, deformed, deformation_field, model, parameters, xc, yc, Weight_cnt, Weight_loc, Weight_val)
;
;;COMPARE TO NUMERICAL GRADIENT
;
;  maximum = max(Grad, ind)
;  control_point = ARRAY_INDICES(Grad,ind)
;;  control_point = [10,1,1,1]
;  GradNum = MOCOMO_2D_UNITTEST_NUMGGRAD(Source, deformation_field, model, parameters, xc, yc, control_point)
;  GradAna = Grad[control_point[0],control_point[1],control_point[2],control_point[3]]
;
;  print, 'Analytical / Numerical Gradient at control point: ', control_point
;  print, GradAna, GradNum
;  print, 'Difference between analytical and numerical gradient (% of analytical)'
;  print, 100*abs(GradAna - GradNum)/abs(GradAna)
;
;  OBJ_DESTROY, Model
;
;END







PRO MOCOMO_2D_GROUPWISE__TEST, SETTINGS=settings

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

  IF N_ELEMENTS(settings) EQ 0 THEN settings = 2 ;(1=development, 2=10min validatiom, 3=3hr validation)

  CASE settings OF
    1:BEGIN
      resolution = 64
      precision = 0.001
      w = window_small
      filename = 'upperpole'
      END
    2:BEGIN
      resolution = 64
      precision = 0.001
      w = window_large
      filename = 'leftkidney'
    END
    3:BEGIN
      resolution = 16
      precision = 0.10
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

  MOCOMO_2D_GROUPWISE, Source, ModelName, Independent, GRID_SIZE=Resolution, TOLERANCE=Precision, WINDOW=w

;PERFORMANCE

  calculation_time = systime(1)-start_time
  Model -> FIT, Source, Target
  ChiSq = total((Source - Target)^2.)
  OBJ_DESTROY, Model

  time = strcompress(calculation_time/60.)
  improvement = strcompress(100*(start_ChiSq-ChiSq)/start_ChiSq)

  print, 'Groupwise'
  print, 'Calculation time (min): ' + time
  print, 'Chi-Square reduction (%): '+ improvement

;EXPORT DEFORMED GIF

  image = BYTE(255*(Source-min(Source))/(0.5*max(Source)-min(Source)))
  File = DataPath + filename + '_groupwise_[' + time + ' min, ' + improvement + ' %]' + '.gif'
  LOADCT, 0
  FOR k=0L,ns[0]-1 DO write_gif, File, REFORM(image[k,*,*]), /MULTIPLE
  write_gif, File, /CLOSE


END






