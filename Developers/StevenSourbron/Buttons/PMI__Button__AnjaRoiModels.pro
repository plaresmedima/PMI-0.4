function PMI__Button__Input__AnjaRoiModels $
, 	top $
,	Status	= id $
, 	Stdy	= Stdy $
,	time 	= time $
,	curve 	= roi $
,	aif		= aif $
,	RoiName	= RoiName

	PMI__Info, top, Status=id, Stdy=Stdy

    Series = Stdy->names(0,ns,DefDim=3,ind=ind,sel=sel)
    Regions = Stdy->names(1,nr)

	v = PMI__Form(top, Title='Perfusion analysis setup', [$
		ptr_new({Type:'DROPLIST',Tag:'series', Label:'Dynamic series', Value:Series, Select:sel}), $
		ptr_new({Type:'DROPLIST',Tag:'aif'	 , Label:'Arterial Region', Value:Regions, Select:stdy->sel(1)}), $
		ptr_new({Type:'DROPLIST',Tag:'roi'	 , Label:'Tissue Region', Value:Regions, Select:stdy->sel(1)}), $
		ptr_new({Type:'VALUE'	,Tag:'nbase' , Label:'Length of baseline (# of dynamics)', Value:8L})])
	IF v.cancel THEN return, 0

	Series = Stdy->Obj(0,ind[v.series])
	Art = Stdy->Obj(1,v.aif)
	Roi = Stdy->Obj(1,v.roi)

	RoiName = Roi->Name()
	Time = Series->c(1)
	Time = Time[1:*]-Time[1]

	Aif = PMI__RoiCurve(Stdy->DataPath(), Series, Art, id, cnt=cnt)
    if cnt eq 0 then begin
    	ok = dialog_message(/information,'Arterial region is empty')
    	return, 0
    end
    Roi = PMI__RoiCurve(Stdy->DataPath(), Series, Roi, id, cnt=cnt)
    if cnt eq 0 then begin
    	ok = dialog_message(/information,'Tissue region is empty')
    	return, 0
    end

	Aif = LMU__Enhancement(Aif[1:*],v.nbase-1,/relative)/0.55
	Roi = LMU__Enhancement(Roi[1:*],v.nbase-1,/relative)

	return, 1
end



pro PMI__Button__Event__AnjaRoiModels, ev

	PMI__Info, ev.top, Stdy=Stdy

	if not PMI__Button__Input__AnjaRoiModels($
		ev.top,	status=status, time=time, curve=roicurve, aif=aif, RoiName= RoiName $
		) then goto, return

	Fit = dblarr(7,n_elements(time))
	Fit[0,*] = time
	Fit[1,*] = roicurve

	Pars = strarr(7,12)
	Pars[0,1:*] = ['Plasma Flow','Plasma Volume','Plasma MTT','Extraction Flow','Interstitial Volume','Interstitial MTT','Extracellular Volume','Extracellular MTT','Exchange Fraction','Arterial Delay','Akaike Weight']
	Pars[1,1:*] = ['ml/100ml/min','ml/100ml','sec','ml/100ml/min','ml/100ml','sec','ml/100ml','sec','%','sec','%']
	Pars[2:*,0] = ['Deconvolution','Tofts','Uptake','Modified Tofts','Exchange ']


	PMI__Message, Status, 'Deconvolving'
	IRF = DeconvolveCurve(time,roicurve,aif,dt=dt,Fit=FitDec,pc='GCV')
	Fit[2,*]=FitDec
	Pars[2,1] = PMI__Round(6000.0*max(IRF),2,/string)
	Pars[2,7] = PMI__Round(100.0*dt*total(IRF),2,/string)
	Pars[2,8] = PMI__Round(dt*total(IRF)/max(IRF),2,/string)

	PMI__Message, Status, 'Fitting 1-compartment model'
	P = [0.3, 120.0/6000] ;[VP+VE, FP]
	Fit[3,*] = FitSingleInlet('Compartment',time,aif,roicurve, P, DELAY_PAR=TA, DELAY_VALUES=[0,20,1], AKAIKE_ERROR=AICone, /POSITIVITY, LIMITED_ABOVE=[1,0])
	Pars[3,1] = PMI__Round(6000D*P[1],2,/string)
	Pars[3,7] = PMI__Round(100D*P[0],2,/string)
	Pars[3,8] = PMI__Round(P[0]/P[1],2,/string)
	Pars[3,10] = PMI__Round(TA,2,/string)


	PMI__Message, Status, 'Fitting 2-compartment model (Uptake)'
	P = [0.1, 120.0/6000, 12/132.] ;[VP, FP, FE/(FP+FE)]
	Fit[4,*] = FitSingleInlet('2CUptakeExchange', time, aif, roicurve, P, DELAY_PAR=TA, DELAY_VALUES=[0,20,1], AKAIKE_ERROR=AICupt, /POSITIVITY, LIMITED_ABOVE=[1,0,1])
	Pars[4,1] = PMI__Round(6000D*P[1],2,/string)
	Pars[4,2] = PMI__Round(100D*P[0],2,/string)
	Pars[4,3] = PMI__Round(1D*P[0]*(1-P[2])/P[1],2,/string)
	Pars[4,4] = PMI__Round(6000D*P[1]*P[2]/(1-P[2]),2,/string)
	Pars[4,9] = PMI__Round(100D*P[2],2,/string)
	Pars[4,10] = PMI__Round(TA,2,/string)


	PMI__Message, Status, 'Fitting 2-compartment model (Tofts)'
	P = [0.3, 2.0/3, 12.0/6000] 	;[VP+VE, VE/(VP+VE), FE]
	Fit[5,*] = FitSingleInlet('ModifiedTofts', time, aif, roicurve, P, DELAY_PAR=TA, DELAY_VALUES=[0,20,1], AKAIKE_ERROR=AICtof, /POSITIVITY, LIMITED_ABOVE=[1,1,0])
	Pars[5,2] = PMI__Round(100D*P[0]*(1-P[1]),2,/string)
	Pars[5,4] = PMI__Round(6000D*P[2],2,/string)
	Pars[5,5] = PMI__Round(100D*P[0]*P[1],2,/string)
	Pars[5,6] = PMI__Round(1D*P[0]*P[1]/P[2],2,/string)
	Pars[5,7] = PMI__Round(100D*P[0],2,/string)
	Pars[5,10] = PMI__Round(TA,2,/string)


	PMI__Message, Status, 'Fitting 2-compartment model (Exchange)'
	P = [0.3, 120.0/6000, 2.0/3, 12/132.] ;[VP+VE, FP, VE/(VP+VE), FE/(FP+FE)]
	Fit[6,*] = FitSingleInlet('Exchange', time, aif, roicurve, P, DELAY_PAR=TA, DELAY_VALUES=[0,20,1], AKAIKE_ERROR=AICexc, /POSITIVITY, LIMITED_ABOVE=[1,0,1,1])
	Pars[6,1] = PMI__Round(6000D*P[1],2,/string)
	Pars[6,2] = PMI__Round(100.0*P[0]*(1-P[2]),2,/string)
	Pars[6,3] = PMI__Round(1D*P[0]*(1-P[2])*(1-P[3])/P[1]	,2,/string)
	Pars[6,4] = PMI__Round(6000D*P[1]*P[3]/(1-P[3]),2,/string)
	Pars[6,5] = PMI__Round(100.0*P[0]*P[2],2,/string)
	Pars[6,6] = PMI__Round(1D*(1-P[3])*P[0]*P[2]/(P[1]*P[3]),2,/string)
	Pars[6,7] = PMI__Round(100.0*P[0],2,/string)
	Pars[6,8] = PMI__Round(P[0]/P[1],2,/string)
	Pars[6,9] = PMI__Round(100D*P[3],2,/string)
	Pars[6,10] = PMI__Round(TA,2,/string)



	W = 100*AkaikeWeights([AICone,AICupt,AICtof,AICexc])

	Pars[3,11] = PMI__Round(W[0],2,/string)
	Pars[4,11] = PMI__Round(W[1],2,/string)
	Pars[5,11] = PMI__Round(W[2],2,/string)
	Pars[6,11] = PMI__Round(W[3],2,/string)


	PMI__Control, ev.top, Viewer = 'PMI__Display__ProstatePerfusionRoiOutput', Display=Display

	Display -> Set, /Refresh $
	,	Fit = Fit $
	,	RoiName = RoiName $
	,	Parameters = Pars

	close, 1

	return: PMI__Message, status
end

pro PMI__Button__Control__AnjaRoiModels, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 1)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end


function PMI__Button__AnjaRoiModels, parent,value=value, separator=separator

	SingleInletCompartment
	SingleInlet2CUptakeExchange
	SingleInletModifiedTofts
	SingleInletExchange
	PMI__Display__ProstatePerfusionRoiOutput__Define

	if n_elements(value) eq 0 then value = 'Fit prostate models (ROI)'

	id = widget_button(parent $
	,	value 		= value $
	,	event_pro 	= 'PMI__Button__Event__AnjaRoiModels'$
	,	pro_set_value 	= 'PMI__Button__Control__AnjaRoiModels' $
	, 	separator 	= separator	)

	return, id

end
