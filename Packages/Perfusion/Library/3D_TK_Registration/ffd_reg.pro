FUNCTION ffd_reg, Source, Target, B, step=step, itmax=itmax

if n_elements(itmax) eq 0 then itmax = 4.
if n_elements(step) eq 0 then step = 0.1

Breg = B

 ffd_precompute,Source,size(REFORM(B[0,*,*,*]), /dimensions)
 ffd_grad_precompute, size(REFORM(B[0,*,*,*]), /dimensions)

 for it=1L, itmax do begin
	 G = ffd_grad(Source, Target, Breg)
	 Breg = ffd_lsearch(Source, Target, Breg, G, conv=converged,step=step)
	 if converged then return, Breg
 endfor
  return, Breg

END


