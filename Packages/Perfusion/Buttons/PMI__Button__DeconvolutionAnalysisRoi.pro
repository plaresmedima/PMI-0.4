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


function PMI__Button__Input__DeconvolutionAnalysisRoi $
, 	top $
,	Status	 = id $
,	time	 = time $
,	curve 	 = roicurve $
,	aif 	 = aif $
,	units 	 = units $
,	ROI 	 = roi $
,	nb=nb $
,	ev = ev

	PMI__Info, top, Status=id, Stdy=Stdy
	PMI__Message, id, 'Getting input..'

   	Series = Stdy->Names(0,DefDim=3,ind=ind,sel=sel)
    Units = ['Linear (a.u.)','Linear (%)','DSC-MRI']

	in = PMI__Form(ev.top, Title='Perfusion analysis setup', [$
		ptr_new({Type:'DROPLIST',Tag:'ser', Label:'Dynamic series', Value:Series, Select:sel}), $
		ptr_new({Type:'DROPLIST',Tag:'aif', Label:'Arterial Region', Value:Stdy->names(1), Select:Stdy->sel(1)}), $
		ptr_new({Type:'DROPLIST',Tag:'roi', Label:'Tissue Region', Value:Stdy->names(1), Select:Stdy->sel(1)}), $
		ptr_new({Type:'DROPLIST',Tag:'rel', Label:'Signal model', Value:Units, Select:1}), $
		ptr_new({Type:'VALUE'	,Tag:'nb' , Label:'Length of baseline (# of dynamics)', Value:1B}),$
		ptr_new({Type:'VALUE'	,Tag:'hct', Label:'Patients hematocrit', Value:0.45})])
		IF in.cancel THEN return, 0

    Series = Stdy->Obj(0,ind[in.ser])
    Art = Stdy->Obj(1,in.aif)
    Roi = Stdy->Obj(1,in.roi)
    Units = Units[in.rel]
    nb = in.nb

    Time = Series->c(1)
    Time = Time-Time[0]

    Aif = PMI__RoiCurve(Stdy->DataPath(), Series, Art, id, cnt=cnt)
    if cnt eq 0 then begin
    	ok = dialog_message(/information,'Region <' + Art->Name() + '> is not defined on Series <' + Series->Name() + '>')
    	return, 0
    endif
	RoiCurve = PMI__RoiCurve(Stdy->DataPath(), Series, Roi, id, cnt=cnt)
    if cnt eq 0 then begin
    	ok = dialog_message(/information,'Region <' + Roi->Name() + '> is not defined on Series <' + Series->Name() + '>')
    	return, 0
    endif

	Aif = LMU__Enhancement(Aif,nb,relative=in.rel)/(1-in.hct)
	RoiCurve = LMU__Enhancement(RoiCurve,nb,relative=in.rel)

	return, 1

end

pro PMI__Button__Event__DeconvolutionAnalysisRoi, ev

	PMI__Info, ev.top, stdy=Stdy, Status=status

	PMI__Message, status, 'Getting input..'

  	if not PMI__Button__Input__DeconvolutionAnalysisRoi(ev.top $
     ,	time 	= time $
     ,	curve 	= roicurve $
     ,	aif		= aif $
     ,	Units	= units $
     ,	Roi		= Roi $
     ,	ev		= ev $
     ) then begin
     	PMI__Message, Status
     	return
     endif

	pc = ['LCC','GCV']
	in = PMI__Form(ev.top, Title='Deconvolution analysis setup: advanced options', [$
	ptr_new({Type:'DROPLIST',Tag:'pc', Label:'Parameter-choice method', Value:pc, Select:1}), $
	ptr_new({Type:'VALUE'	,Tag:'m0', Label:'Minimal regularization parameter', Value:0.001}), $
	ptr_new({Type:'VALUE'	,Tag:'m1', Label:'Maximal regularization parameter', Value:1.0}), $
	ptr_new({Type:'VALUE'	,Tag:'nm', Label:'Number of regularization parameters', Value:100L}), $
	ptr_new({Type:'VALUE'	,Tag:'wm', Label:'Parameter smoothing window', Value:1L})])
	IF in.cancel THEN begin
     	PMI__Message, Status
     	return
    endif

	PMI__Message, status, 'Deconvolving..'

	IRF = DeconvolveCurve(time,	roicurve, aif, dt=dt, CurveRegr=c, Fit=Fit, $
		pc=pc[in.pc], wm=in.wm, m0=in.m0, m1=in.m1, nm=in.nm, Quad='O2')

	PMI__Control, ev.top, Viewer = 'PMI__Display__PerfusionRoiOutput', Display=Display

	Display -> Set, /Refresh $
	,	Model = 'Deconvolution (Residue Detection)' $
	,	Time = dt*findgen(n_elements(c)) $
	,	Curve = c $
	,	Fit = Fit $
	,	Units = Units $
	,	RoiName = Roi->Name() $
	,	Parameters = $
		[ {Name:'Plasma Flow'			,Units:'ml/100ml/min'	,Value:6000.0*max(IRF)			,Nr:0,Rnd:1} $
		, {Name:'Mean Transit Time'		,Units:'sec'			,Value:dt*total(IRF)/max(IRF)	,Nr:1,Rnd:1} $
		, {Name:'Volume of Distribution'	,Units:'ml/100ml'		,Value:100.0*dt*total(IRF) 		,Nr:2,Rnd:1} ]

end



pro PMI__Button__Control__DeconvolutionAnalysisRoi, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->names(1,nr)
		sensitive = (ns gt 0) and (nr gt 1)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__DeconvolutionAnalysisRoi, parent,value=value,separator=separator

	PMI__Display__PerfusionRoiOutput__Define

	if n_elements(value) eq 0 then value = 'Deconvolution analysis (ROI) - Residue detection'

    id = widget_button(parent $
    , 	value 		= value $
	, 	event_pro	= 'PMI__Button__Event__DeconvolutionAnalysisRoi'	$
	,	pro_set_value 	= 'PMI__Button__Control__DeconvolutionAnalysisRoi' $
	, 	separator 	= separator	)

	return, id
end

