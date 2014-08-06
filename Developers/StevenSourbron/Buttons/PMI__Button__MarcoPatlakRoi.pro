FUNCTION PMI__Button__Input__MarcoPatlakRoi, top, time, aif, curve, RoiName = RoiName

    PMI__Info, top, Stdy=Stdy
    DynSeries = Stdy->Names(0,DefDim=3,ind=ind,sel=sel)
	in = {ser:sel, aif:stdy->sel(1),roi:stdy->sel(1), nb:1}

	WHILE 1 DO BEGIN

		in = PMI__Form(top, Title='Perfusion analysis setup', [$
		    ptr_new({Type:'DROPLIST',Tag:'ser', Label:'Dynamic series', Value:DynSeries, Select:in.ser}), $
		    ptr_new({Type:'DROPLIST',Tag:'aif', Label:'Arterial Region', Value:Stdy->names(1), Select:in.aif}), $
		    ptr_new({Type:'DROPLIST',Tag:'roi', Label:'Tissue Region', Value:Stdy->names(1), Select:in.roi}), $
		    ptr_new({Type:'VALUE'	,Tag:'nb' , Label:'Length of baseline (# of dynamics)', Value:in.nb})])
		IF in.cancel THEN return, 0

        ;Get times and baseline
    	IF in.nb LT 1 THEN BEGIN
    		in.nb = 1
    		msg = ['Baseline length must be greater or equal to 1','Please select another baseline length']
    		IF 'Cancel' EQ dialog_message(msg,/information,/cancel) THEN BREAK ELSE CONTINUE
    	ENDIF
    	Series = Stdy->Obj(0,ind[in.ser])
    	IF in.nb GT Series->d(3) THEN BEGIN
    		in.nb = 1
    		msg = ['Baseline length must be less than the total number of dynamics','Please select another baseline length']
    		IF 'Cancel' EQ dialog_message(msg,/information,/cancel) THEN BREAK ELSE CONTINUE
    	ENDIF
    	time = Series->t() - Series->t(0)

        ;Get AIF
	   	Aif = PMI__RoiCurve(Stdy->DataPath(), Series, Stdy->Obj(1,in.aif), status, cnt=cnt)
    	IF cnt EQ 0 THEN BEGIN
    		msg = ['Arterial region is empty on this series','Please select another region and/or series']
    		IF 'Cancel' EQ dialog_message(msg,/information,/cancel) THEN BREAK ELSE CONTINUE
    	ENDIF
    	IF n_elements(Aif) NE Series->d(3) THEN BEGIN
    		msg = ['Arterial region is not defined on every dynamic','Please select another region and/or series']
    		IF 'Cancel' EQ dialog_message(msg,/information,/cancel) THEN BREAK ELSE CONTINUE
    	ENDIF
    	Aif = LMU__Enhancement(Aif,1,relative=0)/(1-0.45)

        ;Get tissue conc
 	   	Curve = PMI__RoiCurve(Stdy->DataPath(), Series, Stdy->Obj(1,in.roi), status, cnt=cnt)
    	IF cnt EQ 0 THEN BEGIN
    		msg = ['Tissue region is empty on this series','Please select another region and/or series']
    		IF 'Cancel' EQ dialog_message(msg,/information,/cancel) THEN BREAK ELSE CONTINUE
    	ENDIF
     	Curve = LMU__Enhancement(Curve,1,relative=0)
     	RoiName = (Stdy->Obj(1,in.roi))->Name()
   	    return, 1

  	ENDWHILE
  	return, 0
END

pro PMI__Button__Event__MarcoPatlakRoi, ev

	PMI__Info, ev.top, Stdy=Stdy
    IF NOT PMI__Button__Input__MarcoPatlakRoi(ev.top, time, aif, curve, RoiName=RoiName) THEN RETURN

    MatrixInv = PatlakLinMatrix(time, aif)
	Pars = MatrixInv ## Curve
	vp = Pars[0]
	Ktrans = Pars[1]
    Fit = vp*aif + Ktrans*IntVector(time,aif)

	PMI__Control, ev.top, Viewer = 'PMI__Display__PerfusionRoiOutput', Display=Display

	Display -> Set, /Refresh $
	,	Model = 'Patlak model', Units = 'Signal Enhancement (a.u.)'  $
	,	Time=Time, Curve=Curve,	Fit=Fit, RoiName=RoiName $
	,	Parameters = $
		[{Name:'Intracellular Uptake Rate', Units:'/100/min', Value:6000D*Ktrans, Nr: 0, Rnd:1} $
		,{Name:'Extracellular Volume', Units:'ml/100ml', Value:100D*vp ,Nr: 1, Rnd:1} ]

end

pro PMI__Button__Control__MarcoPatlakRoi, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 1)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__MarcoPatlakRoi, parent,value=value, separator=separator

	PMI__Display__PerfusionRoiOutput__Define

	if n_elements(value) eq 0 then value = 'Fit non-linear Patlak model (ROI)'

	id = widget_button(parent 	$
	,	value 		= value		$
	,	event_pro 	= 'PMI__Button__Event__MarcoPatlakRoi'	$
	,	pro_set_value 	= 'PMI__Button__Control__MarcoPatlakRoi' $
	, 	separator 	= separator	)

	return, id

end
