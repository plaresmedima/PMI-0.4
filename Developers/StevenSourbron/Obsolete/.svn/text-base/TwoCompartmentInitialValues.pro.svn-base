;DESCRIPTION:

;This function provides different sets of initial values to be used for a two-compartment model
;The initial values are defined in the convective representation
;in units of ml/100ml/min (for Flow parameters) and in ml/100ml (for Volume parameters)
;Initial values are returned in a user-defined representation

;Written by Steven Sourbron, Klinikum Grosshadern, LMU, Munich, 07 sept 2007

;;CALLING SEQUENCE: Q = TwoCompartmentInitialValues(set,representation)

;ARGUMENTS:
;
;	set				: an integer specifying which set of intial values is to be returned
;	representation	: A string specifying the representation of the return value
;				      If this argument is not given, the normal representation is returned
;			 		  Possible values: 'Normal', 'Eigenvalue', 'Kinetic', 'Convective', 'Inverse'
;			 		  The string can be abbreviated, e.g. 'Norm', 'Eig', 'Kin', 'Conv' or 'N', 'E', 'K', 'C'


;RETURN VALUE: A 6-element array of model parameters


function TwoCompartmentInitialValues, set, Representation

	if n_elements(representation) eq 0 then representation='N'

	case set of
		0:begin		;Standard Whole Kidney
			F10 = 200.0	;(1-Standard Hematocrit) x (Literature Blood Flow) = 0.55 x 360
			F21 = 40.0	;20 percent of PF (Kidney Paper)
			F12 = 0.0
			V1 = 15.0	;(1-Standard Hematocrit) x (Literature Cortex Blood Volume) = 0.55 x 30
			V2 = 20.0	;guess
			;Note: V2 is the apparent tubular volume VT/(1-f) in the kidney.
			;The exact tubular volume cannot be measured.
		end
		1:begin		;Standard Whole Kidney + Double the volumes
			F10 = 200.0
			F21 = 40.0
			F12 = 0.0
			V1 = 30.0
			V2 = 40.0
		end
		2:begin		;Standard Whole Kidney + Double the flows
			F10 = 400.0
			F21 = 80.0
			F12 = 0.0
			V1 = 15.0
			V2 = 20.0
		end
		3:begin		;Standard Whole Kidney + Double the volumes, half the flows
			F10 = 100.0
			F21 = 20.0
			F12 = 0.0
			V1 = 30.0
			V2 = 40.0
		end
		4:begin		;First Whole Kidney measurement, volunteer 12
			F10 = 288.44007
			F21 = 14.973235
			F12 = 0.0
			V1 = 19.704255
			V2 = 28.468041
		end
		5:begin		;Standard Whole tumor
			F10 = 120.0
			F21 = 12.0
			F12 = F21
			V1 = 10.0
			V2 = 20.0
		end
		6:begin		;Alternative Whole tumor
			F10 = 120.0
			F21 = 12.0
			F12 = F21
			V1 = 20.0
			V2 = 40.0
		end
		7:begin		;Alternative Whole tumor
			F10 = 240.0
			F21 = 24.0
			F12 = F21
			V1 = 10.0
			V2 = 20.0
		end
		8:begin		;Alternative Whole tumor
			F10 = 60.0
			F21 = 6.0
			F12 = F21
			V1 = 20.0
			V2 = 40.0
		end
		9:begin		;First Measurement Whole tumor
			F10 = 129.0
			F21 = 3.6
			F12 = F21
			V1 = 8.8
			V2 = 23.0
		end
		10:begin		;Intermediate regime
			F10 = 120.0
			F21 = 12.0
			F12 = 0.5*F21
			V1 = 10.0
			V2 = 20.0
		end
		11:begin		;Standard Muscle
			F10 = 10.0
			F21 = 10.0
			F12 = F21
			V1 = 5.0
			V2 = 10.0
		end

	endcase

	P = [F21/6000,F12/6000,V1/100,V2/100,F10/6000,0.0]

	return, TwoCompartmentRepresentation(P,'C',representation)
end