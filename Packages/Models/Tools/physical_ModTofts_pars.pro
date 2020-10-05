FUNCTION physical_ModTofts_pars, p

	d = SIZE(p,/DIMENSIONS)
	p = REFORM(p, d[0], PRODUCT(d[1:*]), /OVERWRITE)

	VP = p[2,*]
	VE = p[1,*]/p[0,*] - VP
	Ktrans = p[0,*] * VE

	p_rec = p
	p_rec[0,*] = Ktrans
	p_rec[1,*] = VP
	p_rec[2,*] = VE

	p = REFORM(p, d, /OVERWRITE)
	p_rec = REFORM(p_rec, d, /OVERWRITE)

	RETURN, remove_inf(p_rec)

END