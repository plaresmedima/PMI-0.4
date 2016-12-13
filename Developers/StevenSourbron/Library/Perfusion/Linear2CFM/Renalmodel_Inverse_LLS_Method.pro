;PURPOSE:
; Do the numerical integration and determines the kinetic parameters
; Date: 16/12/2013
;****************************************************************************************************

PRO RENALMODEL_INVERSE_LLS_METHOD,  Ctt, CA, t, TPrec, TTrec, VPrec, VTrec, arec, brec, crec, drec


n = n_elements(t)

;Numerical Integration using the Trapezoidal Rule

Cttint1=TOTAL( (t[1:*]-t) * (Ctt[1:*]+Ctt) / 2.0 , /cumulative)
Cttint1=[0,Cttint1]
Cttint2=TOTAL( (t[1:*]-t) * (Cttint1[1:*]+Cttint1) / 2.0 , /cumulative)
Cttint2=[0,Cttint2]
CAint1=TOTAL( (t[1:*]-t) * (CA[1:*]+CA) / 2.0 , /cumulative)
CAint1=[0,CAint1]
CAint2=TOTAL( (t[1:*]-t) * (CAint1[1:*]+CAint1) / 2.0 , /cumulative)
CAint2=[0,CAint2]

;Cttint1= (t[1]-t[0]) * TOTAL(Ctt, /cumulative)
;Cttint2= (t[1]-t[0]) * TOTAL(Cttint1, /cumulative)
;CAint1= (t[1]-t[0]) * TOTAL(CA, /cumulative)
;CAint2= (t[1]-t[0]) * TOTAL(CAint1, /cumulative)

;Cttint1=Ctt*0 & for i=1L,n-1 do Cttint1[i]= INT_TABULATED(t[0:i],Ctt[0:i])
;Cttint2=Ctt*0 & for i=1L,n-1 do Cttint2[i]= INT_TABULATED(t[0:i],Cttint1[0:i])
;CAint1=Ctt*0 & for i=1L,n-1  do CAint1[i] = INT_TABULATED(t[0:i],CA[0:i])
;CAint2=Ctt*0 & for i=1L,n-1  do CAint2[i] = INT_TABULATED(t[0:i],CAint1[0:i])

A= TRANSPOSE([[Cttint2],[Cttint1],[-CAint1],[-CAint2]])


SVDC, A,W,U,V

Utrans=TRANSPOSE(U)
B2=TRANSPOSE([-Ctt])
Xlls = Utrans##B2
for K=0,3 DO IF W[K] GT 0 then Xlls[k] = Xlls[k]/W[k]
Xlls=V##Xlls

arec= Xlls[0]
brec= Xlls[1]
crec= Xlls[2]
drec= Xlls[3]


;Once the values of Xlls have been obtained, the kinetic parameters TP,TT,VP,VT can be determined from the relations below:

if brec^2/4 GE arec then begin
Tprec = (brec-(brec^2 -4*arec)^(1.0/2.))/(2*arec)
Ttrec = (brec+(brec^2 -4*arec)^(1.0/2.))/(2*arec)
Vprec= crec*(brec-(brec^2 -4*arec)^(1.0/2.))/(2*arec)
Vtrec= drec/arec - Vprec

endif else begin

;If the noise in the input data is too large, then use the relations below:

;val1=brec/2+((2-arec)^(3.0)/27.+ brec^(2.0)/4.)^(1.0/2)
;val1 = val1 ge 0 ? val1^(1/3.0) : -(-val1)^(1/3.0)
;val2=brec/2-((2-arec)^(3.0)/27.+ brec^(2.0)/4.)^(1.0/2)
;val2= val2 ge 0 ? val2^(1/3.0) : -(-val2)^(1/3.0)
;
;TPrec=1.0/(val1+val2)
;TTrec=1.0/(val1+val2)
;VPrec=crec*Tprec
;VTrec=(drec/arec)-Vprec

 TPrec = 2E/brec
 TTrec = 2E/brec
 VPrec =crec*Tprec
 VTrec =(drec/arec)-Vprec



endelse

END