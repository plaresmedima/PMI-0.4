PRO test_pkreg_mres

  test_data, 'EA_14b', time=time, Caif=Ca, Data=Source, Baseline=Baseline

  voxel_sizes = [3.1,3.1,4.]
  matrix_new = size(Source[*,*,0,0],/dimensions)
  Source = resample_isotropic(Source,voxel_sizes, matrix_new)

  sys_t = systime(1)

  Deformed = pkreg_mres(Time, Ca, Source, Baseline)

  Target   = pkfit(Time, Ca, Deformed, Baseline, par=p_rec)

  print, 'Calculation time (min)', (systime(1)-sys_t)/60.

  path = test_path('output')
  openw, 1, path + 'Deformed_EA_14b.dat'
  writeu, 1, Deformed
  close, 1

END


