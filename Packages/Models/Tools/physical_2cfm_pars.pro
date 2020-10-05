FUNCTION PHYSICAL_2CFM_PARS, p

	d = SIZE(p,/DIMENSIONS)
	p = REFORM(p, d[0], PRODUCT(d[1:*]), /OVERWRITE)

	Det = sqrt(p[1,*]^2-4*p[0,*])

	Tp = (p[1,*] - Det)/(2*p[0,*])
	Te = (p[1,*] + Det)/(2*p[0,*])
	Fp = p[2,*]
	Tt = p[3,*]/(Fp*p[0,*])
	Ft = Fp*(Tt-Tp)/Te

    p_rec = p
    p_rec[0,*] = Fp
    p_rec[1,*] = Tp
    p_rec[2,*] = Ft
	p_rec[3,*] = Te

	p = REFORM(p, d, /OVERWRITE)
	p_rec = REFORM(p_rec, d, /OVERWRITE)

	RETURN, remove_inf(p_rec)

END