Pro TwoCompartmentKidney_Plug, X, P, F ;procedure calculates the output conc F TG= TP

	FP = P[0]	;intial flow into the plasma compartment
	TP = P[1]	;mean transit time through plasma compartment
	TT = P[2]	;mean transit time through tubular compartment
	Ex = P[3]	;fraction of FP that passes to the tubular compartment


	n = n_elements(X)/2

 	t = X[0:n-1]

	CP = ExpConvolution(1/TP,X)

  	EP = FP*CP

  	Ef = ((FP*Ex)/TP)*StepConvolution(1/TT,[t,CP])

  	F = EP + Ef

end