;
;
;    Copyright (C) 2009 Steven Sourbron
;
;    This program is free software; you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation; either version 2 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License along
;    with this program; if not, write to the Free Software Foundation, Inc.,
;    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
;
;
;


FUNCTION PMI__Button__Input__FastDeconvolutionAnalysisRoiPVcorr, top, series, aif, roi, in, RoiName

    PMI__Info, top, Stdy=Stdy
    DynSeries = Stdy->Names(0,DefDim=3,ind=ind,sel=sel)
	in = {ser:sel, aif:stdy->sel(1), roi:stdy->sel(1), vof:0, rel:0, nb:10, hct:0.45}

	WHILE 1 DO BEGIN

		in = PMI__Form(top, Title='Perfusion analysis setup', [$
		ptr_new({Type:'DROPLIST',Tag:'ser', Label:'Dynamic series', Value:DynSeries, Select:in.ser}), $
		ptr_new({Type:'DROPLIST',Tag:'aif', Label:'Arterial Region', Value:Stdy->names(1), Select:in.aif}), $
		ptr_new({Type:'DROPLIST',Tag:'roi', Label:'Tissue Region', Value:[Stdy->names(1)], Select:in.roi}), $
		ptr_new({Type:'DROPLIST',Tag:'vof', Label:'Venous Region', Value:['<none>',Stdy->names(1)], Select:in.vof}), $
		ptr_new({Type:'DROPLIST',Tag:'rel', Label:'Approximate tracer concentrations by:', Value:['Signal Enhancement (T1)','Relative Signal Enhancement (T1)','Relative Signal Enhancement (T2)'], Select:in.rel}), $
		ptr_new({Type:'VALUE'	,Tag:'nb' , Label:'Length of baseline (# of dynamics)', Value:in.nb}),$
		ptr_new({Type:'VALUE'	,Tag:'hct', Label:'Patients hematocrit', Value:in.hct})])
		IF in.cancel THEN return, 0

    	Series = Stdy->Obj(0,ind[in.ser])
    	IF (in.nb LT 1) or (in.nb GT Series->d(3)) THEN BEGIN
    		in.nb = 10
    		msg = ['Baseline length must be less than the total number of dynamics','Please select another baseline length']
    		IF 'Cancel' EQ dialog_message(msg,/information,/cancel) THEN BREAK ELSE CONTINUE
    	ENDIF
	   	Aif = PMI__RoiCurve(Stdy->DataPath(), Series, Stdy->Obj(1,in.aif), status, cnt=cnt)
    	IF cnt EQ 0 THEN BEGIN
    		msg = ['Arterial region is empty on this series','Please select another region and/or series']
    		IF 'Cancel' EQ dialog_message(msg,/information,/cancel) THEN BREAK ELSE CONTINUE
    	ENDIF
    	IF n_elements(Aif) NE Series->d(3) THEN BEGIN
    		msg = ['Arterial region is not defined on every dynamic','Please select another region and/or series']
    		IF 'Cancel' EQ dialog_message(msg,/information,/cancel) THEN BREAK ELSE CONTINUE
    	ENDIF
    	Aif = LMU__Enhancement(Aif,in.nb,relative=in.rel)/(1-in.hct)
    	Roi = Stdy->Obj(1,in.roi)
    	RoiName = Roi->Name()
	   	Roi = PMI__RoiCurve(Stdy->DataPath(), Series, Roi, status, cnt=cnt)
    	IF cnt EQ 0 THEN BEGIN
    		msg = ['Tissue region is empty on this series','Please select another region and/or series']
    		IF 'Cancel' EQ dialog_message(msg,/information,/cancel) THEN BREAK ELSE CONTINUE
    	ENDIF
    	IF n_elements(Roi) NE Series->d(3) THEN BEGIN
    		msg = ['Tissue region is not defined on every dynamic','Please select another region and/or series']
    		IF 'Cancel' EQ dialog_message(msg,/information,/cancel) THEN BREAK ELSE CONTINUE
    	ENDIF
    	Roi = LMU__Enhancement(Roi,in.nb,relative=in.rel)
    	IF in.vof EQ 0 THEN return, 1
    	;correct partial volume
    	Vof = PMI__RoiCurve(Stdy->DataPath(), Series, Stdy->Obj(1,in.vof-1), status, cnt=cnt)
        IF cnt EQ 0 THEN BEGIN
    		msg = ['Venous region is empty on this series','Please select another region and/or series']
    		IF 'Cancel' EQ dialog_message(msg,/information,/cancel) THEN BREAK ELSE CONTINUE
    	ENDIF
    	IF n_elements(Vof) NE Series->d(3) THEN BEGIN
    		msg = ['Venous region is not defined on every dynamic','Please select another region and/or series']
    		IF 'Cancel' EQ dialog_message(msg,/information,/cancel) THEN BREAK ELSE CONTINUE
    	ENDIF
    	Vof = LMU__Enhancement(Vof,in.nb,relative=in.rel)
       	time = Series->t()-Series->t(0)
		IRF = DeconvolveCurve(time,	vof, aif, dt=dt, pc='GCV', wm=1L, m0=0.001, m1=1.0, nm=100L, Quad='O2')
		Aif = Aif*dt*total(IRF)
    	return, 1
  	ENDWHILE
  	return, 0
END

pro PMI__Button__Event__DeconvolutionAnalysisRoiPVcorr, ev

	PMI__Info, ev.top, stdy=Stdy, Status=status

	PMI__Message, status, 'Getting input..'

	IF NOT PMI__Button__Input__FastDeconvolutionAnalysisRoiPVcorr(ev.top, series, aif, roi, in, RoiName) THEN return
	time = Series->t() - Series->t(0)

	PMI__Message, status, 'Deconvolving..'

	IRF = DeconvolveCurve(time,	roi, aif, dt=dt, CurveRegr=c, Fit=Fit, pc='GCV', wm=1L, m0=0.001, m1=1.0, nm=100L, Quad='O2')

	PMI__Control, ev.top, Viewer = 'PMI__Display__PerfusionRoiOutput', Display=Display

	Display -> Set, /Refresh $
	,	Model = 'Model-free deconvolution' $
	,	Time = dt*findgen(n_elements(c)) $
	,	Curve = c $
	,	Fit = Fit $
	,	Units = 'Concentration' $
	,	RoiName = RoiName $
	,	Parameters = $
		[ {Name:'Plasma Flow'			,Units:'ml/100ml/min'	,Value:6000.0*max(IRF)			,Nr:0,Rnd:1} $
		, {Name:'Mean Transit Time'		,Units:'sec'			,Value:dt*total(IRF)/max(IRF)	,Nr:1,Rnd:1} $
		, {Name:'Volume of Distribution',Units:'ml/100ml'		,Value:100.0*dt*total(IRF) 		,Nr:2,Rnd:1} ]

end



pro PMI__Button__Control__DeconvolutionAnalysisRoiPVcorr, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->names(1,nr)
		sensitive = (ns gt 0) and (nr gt 1)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__DeconvolutionAnalysisRoiPVcorr, parent,value=value,separator=separator

	PMI__Display__PerfusionRoiOutput__Define

	if n_elements(value) eq 0 then value = 'Deconvolution analysis (ROI) - Residue detection'

    id = widget_button(parent $
    , 	value 		= value $
	, 	event_pro	= 'PMI__Button__Event__DeconvolutionAnalysisRoiPVcorr'	$
	,	pro_set_value 	= 'PMI__Button__Control__DeconvolutionAnalysisRoiPVcorr' $
	, 	separator 	= separator	)

	return, id
end

