pro PMI__Button__Event__GadovistModelROI, ev

  PMI__Info, ev.top, stdy=Stdy

  if not TumorRoiAnalysis__input($
     ev.top $
     ,	Status	= status $
     ,	time 	= time $
     ,	curve 	= curve $
     ,	aif		= aif $
     ,	Units	= units $
     ,	Roi		= Roi $
     , 	ev=ev ) $
	then begin
		PMI__Message, Status
		return
     endif

	PMI__Message, Status, 'Fitting..'

	P0 = [0.3, 120.0/6000] ;[VP+VE, FP]
	Fit0 = FitSingleInlet('Compartment', time, aif, curve, P0, DELAY_PAR=Pd0, DELAY_VALUES=[0,15,1], AKAIKE_ERROR=aic0, /POSITIVITY, LIMITED_ABOVE=[1,0])
	P1 = [0.1, 120.0/6000, 12/132.] ;[VP, FP, FE/(FP+FE)]
	Fit1 = FitSingleInlet('2CUptakeExchange', time, aif, curve, P1, DELAY_PAR=Pd1, DELAY_VALUES=[0,15,1], AKAIKE_ERROR=aic1, /POSITIVITY, LIMITED_ABOVE=[1,0,1])
	P2 = [0.3, 120.0/6000, 2.0/3, 12/132.] ;[VP+VE, FP, VE/(VP+VE), FE/(FP+FE)]
	Fit2 = FitSingleInlet('Exchange', time, aif, curve, P2, DELAY_PAR=Pd2, DELAY_VALUES=[0,15,1], AKAIKE_ERROR=aic2, /POSITIVITY, LIMITED_ABOVE=[1,0,1,1])

	PMI__Control, ev.top, Viewer = 'PMI__Display__PerfusionRoiOutput', Display=Display

	CASE min([aic0,aic1,aic2]) OF
		aic0:BEGIN
			Fit=Fit0
			Parameters = $
			[{Name:'Plasma Flow'			,Units:'ml/100ml/min'	,Value:6000D*P0[1]	,Nr: 0,Rnd:1} $
			,{Name:'Extracellular Volume'	,Units:'ml/100ml'		,Value:100D*P0[0]	,Nr: 1,Rnd:1} $
			,{Name:'Mean Transit Time'		,Units:'sec'			,Value:1D*P0[0]/P0[1]	,Nr: 2,Rnd:1} $
			,{Name:'Arterial Delay'			,Units:'sec'			,Value:double(Pd0)	,Nr: 9,Rnd:1} ]
		END
		aic1:BEGIN
			Fit=Fit1
			Parameters = $
			[{Name:'Plasma Flow'		,Units:'ml/100ml/min'	,Value:6000D*P1[1]	,Nr: 0,Rnd:1} $
			,{Name:'Plasma Volume'		,Units:'ml/100ml'		,Value:100D*P1[0] 	,Nr: 1,Rnd:1} $
			,{Name:'Plasma Mean Transit Time'			,Units:'sec'			,Value:1D*P1[0]*(1-P1[2])/P1[1]	,Nr: 2,Rnd:1} $
			,{Name:'Extraction Flow'	,Units:'ml/100ml/min'	,Value:6000D*P1[1]*P1[2]/(1-P1[2])	,Nr: 4,Rnd:1} $
			,{Name:'Exchange Fraction'	,Units:'%'				,Value:100D*P1[2]	,Nr: 5,Rnd:1} $
			,{Name:'Arterial Delay'		,Units:'sec'			,Value:Double(Pd1)	,Nr: 9,Rnd:1} ]
		END
		aic2:BEGIN
			Fit=Fit2
			Parameters = $
			[{Name:'Plasma Flow'					,Units:'ml/100ml/min'	,Value:6000D*P2[1]			,Nr: 0,Rnd:1} $
			,{Name:'Plasma Volume'					,Units:'ml/100ml'		,Value:100.0*P2[0]*(1-P2[2])	,Nr: 1,Rnd:1} $
			,{Name:'Plasma Mean Transit Time'		,Units:'sec'			,Value:1D*P2[0]*(1-P2[2])*(1-P2[3])/P2[1]	,Nr: 2,Rnd:1} $
			,{Name:'Extraction Flow'				,Units:'ml/100ml/min'	,Value:6000D*P2[1]*P2[3]/(1-P2[3])	,Nr: 4,Rnd:1} $
			,{Name:'Exchange Fraction'				,Units:'%' 				,Value:100D*P2[3]				,Nr: 5,Rnd:1} $
			,{Name:'Interstitial Volume'			,Units:'ml/100ml'		,Value:100.0*P2[0]*P2[2]		,Nr: 6,Rnd:1} $
			,{Name:'Interstitial Mean Transit Time'	,Units:'sec'			,Value:1D*(1-P2[3])*P2[0]*P2[2]/(P2[1]*P2[3])	,Nr: 7,Rnd:1} $
			,{Name:'Arterial Delay'					,Units:'sec'			,Value:Double(Pd2)	,Nr: 9,Rnd:1} ]
		END
	ENDCASE

	Display -> Set, /Refresh $
	,	Model = 'Gadovist model' $
	,	Time = Time $
	,	Curve = Curve $
	,	Fit = Fit $
	,	Units = Units $
	,	RoiName = Roi->Name() $
	,	Parameters = Parameters

end

pro PMI__Button__Control__GadovistModelROI, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 1)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__GadovistModelROI, parent,value=value, separator=separator

	SingleInletCompartment
	SingleInlet2CUptakeExchange
	SingleInletExchange
	PMI__Display__PerfusionRoiOutput__Define

	if n_elements(value) eq 0 then value = 'Fit single-inlet model with delay (ROI)'

	id = widget_button(parent 						$
	,	value 		= value		$
	,	event_pro 	= 'PMI__Button__Event__GadovistModelROI'	$
	,	pro_set_value 	= 'PMI__Button__Control__GadovistModelROI' $
	, 	separator 	= separator						)
	return, id
end
