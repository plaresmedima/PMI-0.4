FUNCTION ffd_chisq, Source, Target, B

  ffd_precompute, Source, size(REFORM(B[0,*,*,*]), /dimensions)
  Deformed = ffd(Source, B)

  RETURN, 100*total((Target-Deformed)^2.)/total(Target^2.)
END

