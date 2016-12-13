Pro FitDualInletExchangeModel, X, P, F, Fi

	;P = [FA,FV,Kpos,Kneg,Eneg]

	n = n_elements(X)/3

	XA = X[0:2*n-1]
	XV = [X[0:n-1],X[2*n:3*n-1]]

	EApos = ExpConvolution(P[2],XA,Der=DApos)
	EAneg = ExpConvolution(P[3],XA,Der=DAneg)
	EVpos = ExpConvolution(P[2],XV,Der=DVpos)
	EVneg = ExpConvolution(P[3],XV,Der=DVneg)

	F = P[0]*(1-P[4])*EApos + P[0]*P[4]*EAneg + P[1]*(1-P[4])*EVpos + P[1]*P[4]*EVneg

	F0 = (1-P[4])*EApos + P[4]*EAneg
	F1 = (1-P[4])*EVpos + P[4]*EVneg
	F2 = P[0]*(1-P[4])*DApos + P[1]*(1-P[4])*DVpos
	F3 = P[0]*P[4]*DAneg + P[1]*P[4]*DVneg
	F4 = -P[0]*EApos + P[0]*EAneg - P[1]*EVpos + P[1]*EVneg

	Fi = [[F0],[F1],[F2],[F3],[F4]]
end

function FitDualInletExchange, time, curve, aif, vif, Pars=P, AIC=AIC, constrained=constrained, _EXTRA=e

	if n_params() eq 0 then return, 0

	;P = [FA,FV,FE,VP,VE]

	FP = P[0]+P[1]
	KP = (FP+P[2])/P[3]
	KE = P[2]/P[4]
	KB = FP/P[3]

	D = Sqrt((KP+KE)^2-4*KE*KB)
	Kpos = 0.5*(KP+KE+D)
	Kneg = 0.5*(KP+KE-D)
	Eneg = (Kpos-KB)/(Kpos-Kneg)

	P = [P[0],P[1],Kpos,Kneg,Eneg]


  	if keyword_set(constrained) then begin
  		parinfo = replicate({limited:[0,0], limits:[0.D,0]}, 5)
  		parinfo[*].limited[0]=1
  	endif
    Fit = mpcurvefit([time,aif,vif],curve,1+curve*0,P,FUNCTION_name='FitDualInletExchangeModel',parinfo=parinfo,_EXTRA=e)


	if P[2] ge P[3] then begin
		Kpos = P[2]
		Kneg = P[3]
		Eneg = P[4]
	endif else begin
		Kpos = P[3]
		Kneg = P[2]
		Eneg = 1-P[4]
	endelse

	KB = Kpos - Eneg*(Kpos-Kneg)
	KE = Kpos*Kneg/KB
	KP = Kpos+Kneg-KE

	FP = P[0]+P[1]
	FE = FP*(KP/KB-1)
	VP = FP/KB
	VE = FE/KE

	P = [P[0],P[1],FE,VP,VE]

	if arg_present(AIC) then begin
		n = n_elements(Curve)
		AIC = n*alog(total((Curve-Fit)^2)/n) + 2D*(1+n_elements(P))
	endif


	return, Fit
end