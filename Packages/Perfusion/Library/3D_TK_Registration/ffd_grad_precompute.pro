PRO ffd_grad_precompute, nb

  COMMON ffd_grad_const, Weight, Weight_cnt, Weight_loc
  COMMON ffd_const, pi, qi, ri, si, pi2, qi2, ri2, si2, p, q, r, s, p2, q2, r2, s2

  n = nb[0]*nb[1]*nb[2]
  ns = n_elements(p)
  Weight_cnt = lonarr(n)
  Weight_loc = lonarr(8*ns)
  Weight = fltarr(8*ns)
  Bdummy = fltarr(n)
  i0=0L

  FOR i=0L,n-1 DO BEGIN
    Bdummy[i] = 1
    Wi = p*Bdummy[pi] + q*Bdummy[qi] + r*Bdummy[ri] + s*Bdummy[si] + $
         p2*Bdummy[pi2] + q2*Bdummy[qi2] + r2*Bdummy[ri2] + s2*Bdummy[si2]
    Bdummy[i] = 0
	Wi_loc = where(Wi ne 0, Wi_cnt)
	Weight_cnt[i] = Wi_cnt
	Weight_loc[i0:i0+Wi_cnt-1] = Wi_loc
	Weight[i0:i0+Wi_cnt-1] = Wi[Wi_loc]
	i0 = i0+Wi_cnt
  ENDFOR

END