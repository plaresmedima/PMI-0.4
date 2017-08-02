pro pkreg_pro, Time, Ca, Source, Baseline, B=B,itmax=itmax, step=step

  if n_elements(itmax) eq 0 then itmax=4.

  Nt = n_elements(Source[0,0,0,*])

  Deformed = Source

  FOR t=0L, Nt-1 DO BEGIN
      ffd_precompute, REFORM(Source[*,*,*,t]), size(REFORM(B[0,*,*,*,0]), /dimensions)
      Deformed[*,*,*,t] = ffd(REFORM(Source[*,*,*,t]), REFORM(B[*,*,*,*,t]))
  ENDFOR

  FOR it=1L,itmax DO BEGIN

    Target = pkfit(Time, Ca, Deformed, Baseline)

    FOR t=0L, Nt-1 DO BEGIN
      ffd_precompute, REFORM(Source[*,*,*,t]), size(REFORM(B[0,*,*,*,0]), /dimensions)
      B[*,*,*,*,t] = ffd_reg(REFORM(Source[*,*,*,t]), REFORM(Target[*,*,*,t]), REFORM(B[*,*,*,*,t]), step=step)
      Deformed[*,*,*,t] = ffd(REFORM(Source[*,*,*,t]), REFORM(B[*,*,*,*,t]))
    ENDFOR

  ENDFOR

END