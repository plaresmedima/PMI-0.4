PRO IVIM_Parameters, Par, M

 d = size(Par,/dimensions)
 Par = REFORM(Par,d[0]*d[1],8,/OVERWRITE)
 M = FLTARR(d[0]*d[1],4)

 ;0: S0
 ;1: ADCslow
 ;2: ADCfast
 ;3: FastFrac

 M[*,0] = Par[*,0] + Par[*,4]

 ADC1 = TOTAL(Par[*,1:3],2)/3
 ADC2 = TOTAL(Par[*,5:7],2)/3

 M[*,1] = ADC1
 M[*,2] = ADC2
 M[*,3] = Par[*,4]/M[*,0]

 i = WHERE(ADC1 GT ADC2, n)

 IF n GT 0 THEN BEGIN
 	M[i,1] = ADC2[i]
 	M[i,2] = ADC1[i]
 	M[i,3] = 1-M[i,3]
 	x = Par[i,0:3]
 	Par[i,0:3] = Par[i,4:7]
 	Par[i,4:7] = x
 ENDIF

 Par = REFORM(Par,[d[0],d[1],8],/OVERWRITE)
 M = REFORM(M,[d[0],d[1],4],/OVERWRITE)

END