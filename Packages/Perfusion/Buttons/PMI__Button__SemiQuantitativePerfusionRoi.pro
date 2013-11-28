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


function PMI__Button__Input__SemiQuantitativePerfusionRoi $
,	ev $
,	status 	= status $
,	time 	= time $
,	curve 	= roicurve $
,	RoiName	= RoiName $
,	Win		= Win $
,	Units	= Units $
,	BAT = BAT

	PMI__Info, ev.top, Status=status, Stdy=Stdy

    Series = Stdy->Names(0,DefDim=3,ind=ind,sel=sel)
    Units = ['Linear (a.u.)','Linear (%)','DSC-MRI']

	in = PMI__Form(ev.top, Title='Perfusion analysis setup', [$
		ptr_new({Type:'DROPLIST',Tag:'ser', Label:'Dynamic series', Value:Series, Select:sel}), $
		ptr_new({Type:'DROPLIST',Tag:'roi', Label:'Tissue Region', Value:Stdy->names(1), Select:Stdy->sel(1)}), $
		ptr_new({Type:'DROPLIST',Tag:'rel', Label:'Signal model:', Value:Units, Select:0}), $
		ptr_new({Type:'VALUE'	,Tag:'nb' , Label:'Length of baseline (# of dynamics)', Value:1L}),$
		ptr_new({Type:'VALUE'	,Tag:'win', Label:'Smoothing window', Value:5L})])
	IF in.cancel THEN return, 0

	Series = Stdy->Obj(0,ind[in.ser])
	Roi = Stdy->Obj(1,in.roi)
	Units = Units[in.rel]

    Time = Series->c(1)
    Time = Time-Time[0]

	nb = in.nb
	if (nb le 0) or (nb ge Series->d(3)) then nb=1
	BAT = Time[nb-1]

	Win	= in.win
	if Win gt n_elements(time) then Win=1
	RoiName = Roi->Name()
	RoiCurve = PMI__RoiCurve(Stdy->DataPath(),Series,Roi,status,cnt=cnt)
	if cnt eq 0 then return, 0

	return, 1
end


pro PMI__Button__Event__SemiQuantitativePerfusionRoi, ev

	on_error, 2
	CATCH, Error_status
   	IF Error_status NE 0 THEN BEGIN
   	  ok = dialog_message(/error, !ERROR_STATE.MSG)
      CATCH, /CANCEL
   	ENDIF

	PMI__Info, ev.top, Stdy=Stdy

	if not PMI__Button__Input__SemiQuantitativePerfusionRoi($
		ev $
	,	status 	= status $
	,	time 	= time $
	,	curve 	= curve $
	,	RoiName	= RoiName $
	,	Win		= Win $
	,	Units	= Units $
	,	BAT	= BAT ) $
	then begin
		PMI__Message, Status
		return
     endif

	Fit = SemiQuantitativePerfusionRoi(time,curve,BAT,Win=win,Pars=Pars,Units=Units)

	PMI__Control, ev.top, Viewer = 'PMI__Display__PerfusionRoiOutput', Display=Display

	case Units of
		'Linear (a.u.)': U = 'a.u.'
		'Linear (%)': U = '%'
		'DSC-MRI': U = ''
	endcase

	Display -> Set, /Refresh $
	,	Model = 'Semi-quantitative perfusion analysis' $
	,	Time = Time $
	,	Curve = Curve $
	,	Fit = Fit $
	,	Units = Units $
	,	RoiName = RoiName $
	,	Parameters = $
		[{Name:'S0'						,Units:''				,Value:Pars[8]	,Nr: 0,Rnd:2} $
		,{Name:'SNR0'					,Units:''				,Value:Pars[6] 	,Nr: 1,Rnd:2} $
		,{Name:'CNR'					,Units:''				,Value:Pars[7]	,Nr: 2,Rnd:2} $
		,{Name:'Maximum'				,Units:'('+U+')'		,Value:Pars[0]	,Nr: 4,Rnd:2} $
		,{Name:'Time To Maximum'		,Units:'sec'			,Value:Pars[4]	,Nr: 5,Rnd:2} $
		,{Name:'Maximum slope'			,Units:'('+U+') / sec' 	,Value:Pars[1]	,Nr: 6,Rnd:2} $
		,{Name:'Time To Maximum slope'	,Units:' sec'			,Value:Pars[5]	,Nr: 7,Rnd:2} $
		,{Name:'Area under the Curve'	,Units:'('+U+') * sec'	,Value:Pars[2]	,Nr: 8,Rnd:2} $
		,{Name:'Area/Maximum'			,Units:'sec'			,Value:Pars[3]	,Nr: 9,Rnd:2} ]

end

pro PMI__Button__Control__SemiQuantitativePerfusionRoi, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->names(1,nr)
		sensitive = (ns gt 0) and (nr gt 0)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__SemiQuantitativePerfusionRoi, parent, value=value, separator=separator

	PMI__Display__PerfusionRoiOutput__Define

	if n_elements(value) eq 0 then value = 'Semi-quantitative parameters (ROI)'

	id = widget_button(parent $
	,	value 		= value	$
	,	event_pro 	= 'PMI__Button__Event__SemiQuantitativePerfusionRoi'$
	,	pro_set_value 	= 'PMI__Button__Control__SemiQuantitativePerfusionRoi' $
	, 	separator 	= separator	)

	return, id

end
