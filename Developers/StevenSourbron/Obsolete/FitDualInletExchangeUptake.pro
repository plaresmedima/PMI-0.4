Pro FitDualInletExchangeUptakeModel, X, P, F, Fi

	;P = [FA,FV,Kpos,Kneg,Eneg,FI]

	n = n_elements(X)/3

	XA = X[0:2*n-1]
	XV = [X[0:n-1],X[2*n:3*n-1]]

	EApos = ExpConvolution(P[2],XA,Der=DApos)
	EAneg = ExpConvolution(P[3],XA,Der=DAneg)
	EVpos = ExpConvolution(P[2],XV,Der=DVpos)
	EVneg = ExpConvolution(P[3],XV,Der=DVneg)

	CE = (P[0]*EAneg+P[1]*EVneg-P[0]*EApos-P[1]*EVpos)/(1/P[3]-1/P[2])/(P[0]+P[1])
	IE 	= IntVector(X[0:n-1],CE)

	F = P[0]*(1-P[4])*EApos + P[0]*P[4]*EAneg + P[1]*(1-P[4])*EVpos + P[1]*P[4]*EVneg + P[5]*IE

	if n_params() lt 4 then return

	F0 = (1-P[4])*EApos + P[4]*EAneg + P[5]*(IntVector(X[0:n-1],EAneg-EApos)/(1/P[3]-1/P[2]) - IE)/(P[0]+P[1])
	F1 = (1-P[4])*EVpos + P[4]*EVneg + P[5]*(IntVector(X[0:n-1],EVneg-EVpos)/(1/P[3]-1/P[2]) - IE)/(P[0]+P[1])
	F2 = P[0]*(1-P[4])*DApos + P[1]*(1-P[4])*DVpos 	- P[5]*(IntVector(X[0:n-1],P[0]*DApos+P[1]*DVpos)/(P[0]+P[1]) - IE/(P[2]^2))/(1/P[3]-1/P[2])
	F3 = P[0]*P[4]*DAneg + P[1]*P[4]*DVneg 			- P[5]*(IntVector(X[0:n-1],P[0]*DAneg+P[1]*DVneg)/(P[0]+P[1]) + IE/(P[3]^2))/(1/P[3]-1/P[2])
	F4 = -P[0]*EApos + P[0]*EAneg - P[1]*EVpos + P[1]*EVneg
	F5 = IE

	Fi = [[F0],[F1],[F2],[F3],[F4],[F5]]
end

function FitDualInletExchangeUptake, time, curve, aif, vif, Pars=P, AIC=AIC, constrained=constrained, _EXTRA=e

	if n_params() eq 0 then return, 0

	;P = [FA,FV,FE,VP,VE,FI]

	FP = P[0]+P[1]
	KP = (FP+P[2])/P[3]
	KE = P[2]/P[4]
	KB = FP/P[3]

	D = Sqrt((KP+KE)^2-4*KE*KB)
	Kpos = 0.5*(KP+KE+D)
	Kneg = 0.5*(KP+KE-D)
	Eneg = (Kpos-KB)/(Kpos-Kneg)

	P = [P[0],P[1],Kpos,Kneg,Eneg,P[5]]

  	if keyword_set(constrained) then begin
  		parinfo = replicate({limited:[0,0], limits:[0.D,0]}, 6)
  		parinfo[*].limited[0]=1
  	endif
    fit = mpcurvefit([time,aif,vif],curve,1+curve*0,P,FUNCTION_name='FitDualInletExchangeUptakeModel',parinfo=parinfo, _EXTRA=e)

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

	P = [P[0],P[1],FE,VP,VE,P[5]]

	if arg_present(AIC) then begin
		n = n_elements(Curve)
		AIC = n*alog(total((Curve-Fit)^2)/n) + 2D*(1+n_elements(P))
	endif

	return, Fit
end