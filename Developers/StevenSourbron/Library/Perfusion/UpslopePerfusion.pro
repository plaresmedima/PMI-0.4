function UpslopePerfusion,time,curve,aif, mtt=mtt

	F = max(deriv(time,curve)) / max(aif)

	C0 = int_tabulated(time,curve)
	A0 = int_tabulated(time,aif)

	V = C0 / A0

	if not arg_present(mtt) then return, [F,V]

	;total mtt estimated from a two-compartment model

	C1 = int_tabulated(time,time*curve)
	A1 = int_tabulated(time,time*aif)

	mtt = (C1/C0) - (A1/A0)

	return, [F,V]
end