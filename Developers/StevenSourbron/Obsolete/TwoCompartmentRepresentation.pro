
;DESCRIPTION:

;This function implements the transformations between
;the (essentially) equivalent representations of the two-compartment model.
;Derivations, notations and formulae can be found in Sourbron et al (...).
;
;Using the notations of Sourbron et al (...), the three representations are defined as follows:
;
; 	Normal: 		P = [K21,K12,K01,K02,F10,F20]
; 	Eigenvalue: 	P = [K+ ,K- ,K01,K02,F10,F20]
; 	Inverse: 		P = [K+ ,K- ,K01,E12,F10,F20]
; 	Kinetic: 		P = [E21,E12,T1 ,T2 ,F10,F20]
; 	Convective: 	P = [F21,F12,V1 ,V2 ,F10,F20]
;
;Written by Steven Sourbron, Klinikum Grosshadern, LMU, Munich, 05 sept 2007


;CALLING SEQUENCE: Q = TwoCompartmentRepresentation(P, Input, Output)


;ARGUMENTS:
;
;	P		: A 4- or 6- element array of model parameters.
;			 - The last two elements [F10,F20] must be given when transforming
;			   from- or to the convective representation.
;			   In all other transformations they are optional and ignored.
;			 - All values of P must be non-negative
;			 - In the eigenvalue representation, K- must be smaller or equal to K+, and K01 may not be equal to K02
;			 - In the kinetic representation the extraction fractions E21 and E12 must not exceed 1

;	Input	: A String specifying the representation of the argument P
;			 - Possible values: 'Normal', 'Eigenvalue', 'Kinetic', 'Convective'
;			 - The string can be abbreviated, e.g. 'Norm', 'Eig', 'Kin', 'Conv' or 'N', 'E', 'K', 'C'

;	Output	: A String specifying the representation of the return value
;			 - Possible values: 'Normal', 'Eigenvalue', 'Kinetic', 'Convective'
;			 - The string can be abbreviated, e.g. 'Norm', 'Eig', 'Kin', 'Conv'  or 'N', 'E', 'K', 'C'
;
;All three arguments are required


;KEYWORD PARAMETERS:

;	Positive: when set, selects the solution V2>V1 in converting from the inverse representation



;RETURN VALUE: An array of model parameters with equal numbers of elements as P (4 or 6)



function TwoCompartmentRepresentation, P, Input, Output, positive=positive

	Out = strmid(Output,0,1)
	In = strmid(Input,0,1)

	if In eq Out then return, P


	if In eq 'N' then begin

		;P = [K21,K12,K01,K02,F10,F20]

		K1 = P[0]+P[2]
		K2 = P[1]+P[3]

		case Out of

			'E':begin ;Output = Eigenvalue Representation

				;P = [K21,K12,K01,K02,F10,F20]
				Kdif = sqrt((K1-K2)^2+4*P[0]*P[1])
				Ksum = K1+K2
				Kp = 0.5*(Ksum+Kdif)
				Km = 0.5*(Ksum-Kdif)
				Q = P
				Q[0:1] = [Kp,Km]
				;Q = [K+,K-,K01,K02,F10,F20]
			end

			'I':begin ;Output = Inverse Representation

				;P = [K21,K12,K01,K02,F10,F20]
				E12 = P[1]/K2
				Q = TwoCompartmentRepresentation(P,'N','E')
				Q[3] = E12
				;Q = [K+,K-,K01,E12,F10,F20]
			end

			'K':begin ;Output = Kinetic Representation

				;P = [K21,K12,K01,K02,F10,F20]
				Q = P
				Q[0:3] = [P[0]/K1,P[1]/K2,1/K1,1/K2]
				;Q = [E21,E12,T1,T2,F10,F20]
			end

			'C':begin ;Output = Convective Representation

				;P = [K21,K12,K01,K02,F10,F20]
				K21 = P[0]
				K12 = P[1]
				F10 = P[4]
				F20 = P[5]
				N = K1*K2-K12*K21
				V1 = (K2*F10+K12*F20)/N
				V2 = (K1*F20+K21*F10)/N
				F12 = K12*V2
				F21 = K21*V1
				Q = P
				Q[0:3] = [F21,F12,V1,V2]
				;Q = [F21,F12,V1,V2,F10,F20]
			end

		endcase

		return, Q

	endif


	if Out eq 'N' then begin

		case In of

			'E': begin

				;P = [K+,K-,K01,K02,F10,F20]
				K01 = P[2]
				K02 = P[3]
				K21 = (P[0]-K01)*(P[1]-K01)/(K02-K01)
				K12 = (P[0]-K02)*(P[1]-K02)/(K01-K02)
				Q = P
				Q[0:1] = [K21,K12]
				;Q = [K21,K12,K01,K02,F10,F20]
			end

			'I':begin

				;P = [K+,K-,K01,E12,F10,F20]

				E12 = P[3]

				if E12 eq 1 then begin

					Q = P
					Q[3] = 0  ;[K+,K-,K01,K02,F10,F20]
					Q = TwoCompartmentRepresentation(Q,'E','N')

				endif else begin

					K01 = P[2]
					X = E12*K01 + (1-E12)*(P[0]+P[1])
					D = X^2 - 4*(1-E12)*P[0]*P[1]
					if keyword_set(positive) then begin
						K2 = 0.5*(X + sqrt(D))/(1-E12)
					endif else begin
						K2 = 0.5*(X - sqrt(D))/(1-E12)
					endelse
					K02 = (1-E12)*K2
					K12 = E12*K2
					K21 = (P[0]-K01)*(P[1]-K01)/(K02-K01)
					Q = P
					Q[0:3] = [K21,K12,K01,K02]

				endelse

				;Q = [K21,K12,K01,K02,F10,F20]
			end

			'K':begin

				;P = [E21,E12,T1,T2,F10,F20]
				K01 = (1-P[0])/P[2]
				K02 = (1-P[1])/P[3]
				K21 = P[0]/P[2]
				K12 = P[1]/P[3]
				Q = P
				Q[0:3] = [K21,K12,K01,K02]
				;Q = [K21,K12,K01,K02,F10,F20]
			end

			'C':begin

				;P = [F21,F12,V1,V2,F10,F20]
				K01 = (P[4]-P[0]+P[1])/P[2]
				K02 = (P[5]-P[1]+P[0])/P[3]
				K21 = P[0]/P[2]
				K12 = P[1]/P[3]
				Q = P
				Q[0:3] = [K21,K12,K01,K02]
				;Q = [K21,K12,K01,K02,F10,F20]
			end

		endcase

		return, Q

	endif


	Q = TwoCompartmentRepresentation(P,In,'N')
	Q = TwoCompartmentRepresentation(Q,'N',Out)

	return, Q

end