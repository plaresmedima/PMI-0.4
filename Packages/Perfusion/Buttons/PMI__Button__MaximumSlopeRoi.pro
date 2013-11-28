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


function PMI__Button__Input__MaximumSlopeRoi $
   ,    ev $
   ,    Stdy = Stdy $
   ,    status = status $
   ,    time = time $
   ,    aif = aif $
   ,	curve = curve $
   ,	Roi =Roi

    PMI__Info, ev.top, Status=Status, Stdy=Stdy

    Series = Stdy->Names(0,DefDim=3,ind=ind,sel=sel)

	in = PMI__Form(ev.top, Title='Perfusion analysis setup', [$
	ptr_new({Type:'DROPLIST',Tag:'ser', Label:'Dynamic series', Value:Series, Select:sel}), $
	ptr_new({Type:'DROPLIST',Tag:'aif', Label:'Arterial Region', Value:Stdy->names(1), Select:stdy->sel(1)}), $
	ptr_new({Type:'DROPLIST',Tag:'roi', Label:'Tissue Region', Value:Stdy->names(1), Select:stdy->sel(1)}), $
	ptr_new({Type:'DROPLIST',Tag:'rel', Label:'Approximate tracer concentrations by:', Value:['Signal Enhancement','Relative Signal Enhancement'], Select:1}), $
	ptr_new({Type:'VALUE'	,Tag:'nb' , Label:'Length of baseline (# of dynamics)', Value:10}), $
	ptr_new({Type:'VALUE'	,Tag:'win', Label:'Smoothing window', Value:1})])
	IF in.cancel THEN return, 0

    Series = Stdy->Obj(0,ind[in.ser])
    Art = Stdy->Obj(1,in.aif)
    Roi = Stdy->Obj(1,in.roi)

    Time = Series->c(1)
    Time = Time-Time[0]

    Aif = PMI__RoiCurve(Stdy->DataPath(), Series, Art, status, cnt=cnt)
    if cnt eq 0 then begin
    	ok = dialog_message(/information,'Arterial region is empty')
    	return, 0
    end
    Aif = LMU__Enhancement(Aif,in.nb,relative=in.rel)/0.55

	Curve = PMI__RoiCurve(Stdy->DataPath(), Series, Roi, status, cnt=cnt)
    if cnt eq 0 then begin
    	ok = dialog_message(/information,'Region <' + Roi->Name() + '> is not defined on Series <' + Series->Name() + '>')
    	return, 0
    endif
    Curve = LMU__Enhancement(Curve,in.nb,relative=in.rel)
    if in.win gt 2 then curve = smooth(curve,in.win,/edge_truncate)

  	return, 1
end



pro PMI__Button__Event__MaximumSlopeRoi, ev

	PMI__Info, ev.top, Stdy=Stdy

	if not PMI__Button__Input__MaximumSlopeRoi(ev $
   	,   Stdy = Stdy $
   	,   status = status $
   	,   time = time $
   	,   aif = aif $
   	,	curve = curve $
   	,	Roi = Roi) $
	then begin
		PMI__Message, Status
		return
     endif

	PMI__Message, status, 'Calculating..'

	Fit = MaximumSlopePerfusion(time, curve, AIF, flow = flow)

	PMI__Control, ev.top, Viewer = 'PMI__Display__PerfusionRoiOutput', Display=Display

	Display -> Set, /Refresh $
	,	Model = 'Maximum slope perfusion' $
	,	Time = Time $
	,	Curve = Curve $
	, 	Aif = Aif $
	,	Fit = Fit $
	,	Units = Units $
	,	RoiName = Roi->Name() $
	,	Parameters = $
		[{Name:'Compartment Flow', Units:'ml/100ml/min', Value:6000D*flow, Nr:0, Rnd:1} ]

end

pro PMI__Button__Control__MaximumSlopeRoi, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 1)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__MaximumSlopeRoi, parent,value=value, separator=separator

	PMI__Display__PerfusionRoiOutput__Define

	if n_elements(value) eq 0 then value = 'Maximum slope perfusion (ROI)'

	id = widget_button(parent 						$
	,	value 		= value		$
	,	event_pro 	= 'PMI__Button__Event__MaximumSlopeRoi'	$
	,	pro_set_value 	= 'PMI__Button__Control__MaximumSlopeRoi' $
	, 	separator 	= separator						)

	return, id

end
