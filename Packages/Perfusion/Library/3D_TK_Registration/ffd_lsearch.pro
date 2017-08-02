function ffd_lsearch, Source, Target, B, G, conv=converged, step=step

  if n_elements(step) eq 0 then step = 0.1 ;precision in pixel-sizes

  converged = 0
  alpha_init = 5.0

  Gnorm = G/sqrt(total(G^2))
  ChiSq_init = ffd_chisq(Source, Target, B)

  alpha_try = alpha_init
  B_try = B - alpha_try*Gnorm
  ChiSq_try = ffd_chisq(Source, Target, B_try)

  while ChiSq_try GE ChiSq_init do BEGIN
	converged = alpha_try LT step
	if converged then return, B
    alpha_try = alpha_try/10E
	B_try = B - alpha_try*Gnorm
	ChiSq_try = ffd_chisq(Source, Target, B_try)
  ENDWHILE

  ;forward until increase in chisq

  ChiSq_curr = ChiSq_try
  B_curr = B_try

  alpha_try += step
  B_try = B - alpha_try*Gnorm
  ChiSq_try = ffd_chisq(Source, Target, B_try)

  while ChiSq_try LT ChiSq_curr do BEGIN
    ChiSq_curr = ChiSq_try
    B_curr = B_try
    alpha_try += step
    B_try = B - alpha_try*Gnorm
    ChiSq_try = ffd_chisq(Source, Target, B_try)
	if alpha_try GT 1E+2 then break
  ENDWHILE

  return, B_curr

end