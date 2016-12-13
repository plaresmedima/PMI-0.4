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


PRO PMI__Button__FitSingleInletRoi__Display::Fit

	Self->GET, Model=Model, Positivity=Pos, Delay=Delay, Time=Time, AifCurve=Aif, RoiCurve=Curve, OnDisplay=OnDisplay
	Self->SET, Message='Fitting...', Sensitive=0

	IF Delay NE 0 THEN BEGIN
	;	If keyword_set(Pos) then Delay0=0 Else Delay0=-20
		Delay0=0
		DELAY_VALUES=[Delay0,20,time[1]/2]
	ENDIF

	CASE Model OF

		'Maximum slope':begin
			Fit = MaximumSlopePerfusion(time, curve, AIF, flow = flow)
			aic = n_elements(time)*alog(total((curve-Fit)^2)/n_elements(time)) + 2D*(1+3)
			Parameters = $
				[{Name:'Plasma Flow'		,Units:'ml/100ml/min'	,Value:6000D*flow     ,Nr: 0}	]
			Pd=0E
			end

		'Uptake':begin
			P = [12.0/6000] ;[F]
			Fit = FitSingleInlet('Uptake', time, aif, curve, P, DELAY_PAR=Pd, DELAY_VALUES=DELAY_VALUES, AKAIKE_ERROR=aic, POSITIVITY=Pos, /NODERIVATIVE)
			Parameters = $
				[{Name:'Plasma Flow'	,Units:'ml/100ml/min'	,Value:6000D*P[0]	,Nr: 0} ]
			end

		'Steady State':begin
			P = [0.2] ;[V]
			Fit = FitSingleInlet('SteadyState', time, aif, curve, P, DELAY_PAR=Pd, DELAY_VALUES=DELAY_VALUES, AKAIKE_ERROR=aic, POSITIVITY=Pos, /NODERIVATIVE, LIMITED_ABOVE=[1])
			Parameters = $
				[{Name:'Extracellular volume'		,Units:'ml/100ml'		,Value:100D*P[0]	,Nr: 11} ]
			end

		'Patlak':begin
			P = [0.1, 12.0/6000] ;[VP, FE]
			Fit = FitSingleInlet('Patlak', time, aif, curve, P, DELAY_PAR=Pd, DELAY_VALUES=DELAY_VALUES, AKAIKE_ERROR=aic, POSITIVITY=Pos, /NODERIVATIVE, LIMITED_ABOVE=[1,0])
			Parameters = $
				[{Name:'Plasma Volume'		                ,Units:'ml/100ml'		,Value:100D*P[0]	,Nr: 1} $
				,{Name:'Permeability-surface area product'	,Units:'ml/100ml/min'	,Value:6000D*P[1]	,Nr: 4} $
				,{Name:'Ktrans'	                            ,Units:'ml/100ml/min'	,Value:6000D*P[1]	,Nr: 8} ]
			end

		'Model-free':begin
			IRF = DeconvolveCurve(time,	curve, aif, dt=dt, Fit=Fit, pc='GCV', wm=1L, m0=0.001, m1=1.0, nm=100L, Quad='O2')
			Fit = interpol(Fit, dt*findgen(n_elements(Fit)), Time)
			aic = n_elements(time)*alog(total((curve-Fit)^2)/n_elements(time)) + 2D*(1+2)
			Parameters = $
				[{Name:'Plasma Flow'               ,Units:'ml/100ml/min'	,Value:6000D*max(IRF)	         ,Nr: 0} $
				,{Name:'Extracellular Volume'	   ,Units:'ml/100ml'		,Value:100D*dt*total(IRF)	     ,Nr: 11} $
				,{Name:'Extracellular MTT'		   ,Units:'sec'			    ,Value:1D*dt*total(IRF)/max(IRF) ,Nr: 12} $
			]
			Pd=0E
			end

		'Compartment':begin
			P = [0.3, 120.0/6000] ;[V, F]
			Fit = FitSingleInlet('Compartment', time, aif, curve, P, DELAY_PAR=Pd, DELAY_VALUES=DELAY_VALUES, AKAIKE_ERROR=aic, POSITIVITY=Pos, /NODERIVATIVE, LIMITED_ABOVE=[1,0])
			Parameters = $
				[{Name:'Plasma Flow'	         ,Units:'ml/100ml/min'	,Value:6000D*P[1]	 ,Nr: 0} $
				,{Name:'Extracellular Volume'	,Units:'ml/100ml'		,Value:100D*P[0]	 ,Nr: 11} $
				,{Name:'Extracellular MTT'	    ,Units:'sec'			,Value:1D*P[0]/P[1]  ,Nr: 12} $
			]
			end

		'Tofts':begin
			P = [0.3, 120.0/6000] ;[V, F]
			Fit = FitSingleInlet('Compartment', time, aif, curve, P, DELAY_PAR=Pd, DELAY_VALUES=DELAY_VALUES, AKAIKE_ERROR=aic, POSITIVITY=Pos, /NODERIVATIVE, LIMITED_ABOVE=[1,0])
			Parameters = $
				[{Name:'Interstitial Volume'	,Units:'ml/100ml'		,Value:100D*P[0]	     ,Nr: 5} $
				,{Name:'Ktrans'	                ,Units:'ml/100ml/min'	,Value:6000D*P[1]        ,Nr: 8} $
				,{Name:'kep'                    ,Units:'ml/100ml/min'   ,Value:6000D*P[1]/P[0]   ,Nr: 9} $
				,{Name:'Extracellular Volume'	,Units:'ml/100ml'		,Value:100D*P[0]	     ,Nr: 11} $
			]
			end

		'Modified Tofts':begin
			P = [0.3, 2.0/3, 12.0/6000] 	;[VP+VE, VE/(VP+VE), FE]
			Fit = FitSingleInlet('ModifiedTofts', time, aif, curve, P, DELAY_PAR=Pd, DELAY_VALUES=DELAY_VALUES, AKAIKE_ERROR=aic, POSITIVITY=Pos, /NODERIVATIVE, LIMITED_ABOVE=[1,1,0])
			Parameters = $
				[{Name:'Plasma Volume'			,Units:'ml/100ml'		,Value:100D*P[0]*(1-P[1])       ,Nr: 1} $
				,{Name:'Interstitial Volume'	,Units:'ml/100ml'		,Value:100D*P[0]*P[1]	        ,Nr: 5} $
				,{Name:'Interstitial MTT'		,Units:'sec'			,Value:1D*P[0]*P[1]/P[2]        ,Nr: 6} $
				,{Name:'Ktrans'					,Units:'ml/100ml/min'	,Value:6000D*P[2]	            ,Nr: 8} $
				,{Name:'kep'                    ,Units:'ml/100ml/min'   ,Value:6000D*P[2]/(P[0]*P[1])	,Nr: 9} $
				,{Name:'Extracellular Volume'   ,Units:'%'              ,Value:100D*P[0]	            ,Nr: 11}$
			]
			end

		'Modified Tofts (Linear)':begin
			FitModifiedToftsLinear, time, aif, curve, vp=vp, ve=ve, Ktrans=Ktrans, Fit=Fit, DELAY_PAR=Pd, DELAY_VALUES=DELAY_VALUES, AKAIKE_ERROR=aic, POSITIVITY=Pos, /LIMITED_ABOVE
			Parameters = $
				[{Name:'Plasma Volume'			,Units:'ml/100ml'		,Value:100D*vp	         ,Nr: 1} $
				,{Name:'Interstitial Volume'	,Units:'ml/100ml'		,Value:100D*ve	         ,Nr: 5} $
				,{Name:'Interstitial MTT'		,Units:'sec'			,Value:1D*ve/Ktrans      ,Nr: 6} $
				,{Name:'Ktrans'		            ,Units:'ml/100ml/min'	,Value:6000D*Ktrans	     ,Nr: 8} $
				,{Name:'kep'                    ,Units:'ml/100ml/min'   ,Value:6000D*Ktrans/ve	 ,Nr: 9} $
				,{Name:'Extracellular Volume'   ,Units:'%'              ,Value:100D*(vp+ve)	     ,Nr: 11} $
			]
			end

		'2C Uptake':begin
			P = [0.1, 120.0/6000, 12/132.] ;[VP, FP, FE/(FP+FE)]
			Fit = FitSingleInlet('2CUptakeExchange', time, aif, curve, P, DELAY_PAR=Pd, DELAY_VALUES=DELAY_VALUES, AKAIKE_ERROR=aic, POSITIVITY=Pos, /NODERIVATIVE, LIMITED_ABOVE=[1,0,1])
			Parameters = $
				[{Name:'Plasma Flow'			             ,Units:'ml/100ml/min'	,Value:6000D*P[1]		        ,Nr: 0} $
				,{Name:'Plasma Volume'			             ,Units:'ml/100ml'		,Value:100D*P[0]		        ,Nr: 1} $
				,{Name:'Plasma MTT'				             ,Units:'sec'			,Value:1D*P[0]*(1-P[2])/P[1]    ,Nr: 2} $
				,{Name:'Permeability-surface area product'	 ,Units:'ml/100ml/min'	,Value:6000D*P[1]*P[2]/(1-P[2])	,Nr: 4} $
				,{Name:'Ktrans'		                         ,Units:'ml/100ml/min'	,Value:6000D*P[2]*P[1]			,Nr: 8} $
				,{Name:'Extraction Fraction'	             ,Units:'%' 		    ,Value:100D*P[2]	            ,Nr: 10} $
				]
			end

		'2C Exchange':begin
 			P = [0.3, 0.02, 2.0/3, 0.1] ;[VP+VE, FP, VE/(VP+VE), FE/(FP+FE)]
			Fit = FitSingleInlet('Exchange', time, aif, curve, P, DELAY_PAR=Pd, DELAY_VALUES=DELAY_VALUES, AKAIKE_ERROR=aic, POSITIVITY=Pos, /NODERIVATIVE, LIMITED_ABOVE=[1,0,1,1])
			Parameters = $
			    [{Name:'Plasma Flow'                        ,Units:'ml/100ml/min' ,Value:6000D*P[1]                         ,Nr: 0} $
			    ,{Name:'Plasma Volume'                      ,Units:'ml/100ml'     ,Value:100D*P[0]*(1-P[2])	                ,Nr: 1} $
				,{Name:'Plasma MTT'                         ,Units:'sec'          ,Value:1D*(1-P[3])*P[0]*(1-P[2])/P[1] 	,Nr: 2} $
				,{Name:'Permeability-surface area product'  ,Units:'ml/100ml/min' ,Value:6000D*P[1]*P[3]/(1-P[3])			,Nr: 4} $
				,{Name:'Interstitial Volume'                ,Units:'ml/100ml'     ,Value:100D*P[0]*P[2]			            ,Nr: 5} $
				,{Name:'Interstitial MTT'                   ,Units:'sec'          ,Value:1D*P[0]*P[2]*(1-P[3])/(P[1]*P[3])	,Nr: 6} $
				,{Name:'Ktrans'                             ,Units:'ml/100ml/min' ,Value:6000D*P[3]*P[1]		            ,Nr: 8} $
				,{Name:'kep'                                ,Units:'ml/100ml/min' ,Value:6000D*P[3]*P[1]/(P[0]*P[2])		,Nr: 9} $
				,{Name:'Extraction Fraction'                ,Units:'%'            ,Value:100D*P[3]	                        ,Nr: 10} $
				,{Name:'Extracellular Volume'               ,Units:'%'            ,Value:100D*P[0]	                        ,Nr: 11} $
				,{Name:'Extracellular MTT'                  ,Units:'sec'           ,Value:1D*P[0]/P[1]	                    ,Nr: 12} $
			]
			end
	endcase

;	Parameters = [Parameters,{Name:'Akaike Fit Error', Units:'', Value:AIC, Nr:16} ]
	IF Delay NE 0 THEN $
	Parameters = [Parameters,{Name:'Arterial Delay', Units:'sec', Value:1D*Pd, Nr:14} ]

	self.Curve[2] = ptr_new(Fit)
	self.Parameters = ptr_new(Parameters)
	Self->SET, OnDisplay=OnDisplay, /Sensitive
END

PRO PMI__Button__FitSingleInletRoi__Display::Plot

	Self->GET, OnDisplay=OnDisplay

	top=0.9 & dy=0.04 & x0=0.525 & charsize=1.0 & charthick=1.0

	CASE OnDisplay OF

		'':Self->Set, /Erase

		'ROI':begin
			Self -> GET, Time=Time, RoiCurve=Y, RoiName=RoiName
			Self -> SET, /Erase
 			plot, /nodata, position=[0.1,0.2,0.5,0.9]  $
			, 	[0,max(time)], [min(Y),max(Y)] $
			, 	/xstyle, /ystyle $
			, 	background=255, color=0 $
			, 	xtitle = 'Time (sec)', ytitle=self.Units $
			, 	charsize=1.5, charthick=2.0, xthick=2.0, ythick=2.0
			oplot, time, Y, color=6*16, linestyle=0, thick=2
			xyouts, x0, top-0*dy, 'Region Of Interest: ' + RoiName, color=6*16, /normal, charsize=1.5, charthick=1.5
			end

		'AIF':begin
			Self -> GET, Time=Time, AifCurve=Y, AifName=AifName
			Self -> SET, /Erase
 			plot, time, Y, position=[0.1,0.2,0.5,0.9]  $
			, 	/xstyle, /ystyle $
			, 	background=255, color=0 $
			, 	xtitle = 'Time (sec)', ytitle=self.Units $
			, 	linestyle=0, thick=2 $
			, 	charsize=1.5, charthick=2.0, xthick=2.0, ythick=2.0
			xyouts, x0, top-1*dy, 'Arterial Input Function: ' + AifName	, color=0, /normal, charsize=1.5, charthick=1.5
			end

		'FIT':BEGIN
			Self -> GET, RoiCurve=Curve, Time=Time, Fit=Fit, Model=Model, RoiName=RoiName, AifName=AifName
			Self -> SET, /Erase

 			plot, /nodata, position=[0.1,0.2,0.5,0.9]  $
			, 	[0,max(time)], [min([min(Curve),min(Fit)]),max([max(Curve),max(Fit)])] $
			, 	/xstyle, /ystyle $
			, 	background=255, color=0 $
			, 	xtitle = 'Time (sec)', ytitle=self.Units $
			, 	charsize=1.5, charthick=2.0, xthick=2.0, ythick=2.0
			oplot, time, Curve, color=6*16, linestyle=0, thick=2
			oplot, time, Fit, color=12*16, linestyle=0, thick=2

			xyouts, x0, top-0*dy, 'Region Of Interest: ' + RoiName		, color=6*16, /normal, charsize=1.5, charthick=1.5
			xyouts, x0, top-1*dy, 'Arterial Input Function: ' + AifName	, color=0, /normal, charsize=1.5, charthick=1.5
			xyouts, x0, top-3*dy, 'Tissue Model: ' + Model				, color=12*16, /normal, charsize=1.5, charthick=1.5

			P = *self.Parameters

			for i=0L,n_elements(P)-1 do xyouts $
				, x0, top-dy*(5+P[i].Nr) $
				, P[i].Name + ' = ' + PMI__Round(P[i].Value,3,/string) + ' ' + P[i].Units $
				, color=0, /normal, charsize=charsize, charthick=charthick
			END
	ENDCASE

END




FUNCTION PMI__Button__FitSingleInletRoi__Display::Event, ev

	Uname = widget_info(ev.id,/uname)

	i = where(Uname Eq ['ROI','AIF','FIT']+'bttn', cnt)
	If cnt eq 1 then begin
		Self->SET, OnDisplay=strmid(Uname,0,3), /Refresh
		return, 0B
	endif

	i = where(Uname Eq ['ROI','AIF'], cnt)
	If cnt eq 1 then begin
		ptr_free, Self.Curve[i], Self.Curve[2], self.parameters
		Self->SET, OnDisplay=Uname, /Refresh
		return, 0B
	endif

	i = where(Uname Eq ['FIT','Delay','Positive'], cnt)
	if cnt eq 1 then begin
		ptr_free, Self.Curve[2], self.parameters
	;	Self->SET, OnDisplay='ROI', /Refresh
		Self->SET, /Refresh
		return, 0B
	endif

	i = where(Uname Eq ['Export','Export As'], cnt)
	if cnt eq 1 then begin
		Self->GET, Time=Time, RoiCurve=RoiCurve, Fit=Fit, Model=Model, Roiname=Roiname
		if Uname eq 'Export' then begin
			PMI__Info, ev.top, Stdy=Stdy
			Path = Stdy->Datapath() + 'Single-Inlet Models (ROI)'
			file_mkdir, Path
			File = Path + '\' + Roiname + '__SI_' + Model
		endif else begin
			PMI__Info, ev.top, State=State
			if not State -> Get_file(file, file=cleanstr(Roiname + '__' + Model), title='Save as..', filter='.tif') then return,0B
			File = strmid(file,0,strlen(file)-4)
		endelse
		Write_tiff, File + '.tif', reverse(tvrd(/true),3)
		PMI__WritePlot, File + '__Curve.txt', Time, RoiCurve
		PMI__WritePlot, File + '__Fit.txt', time, Fit
		PMI__WritePlot, File + '__Parameters.txt' $
		, 	Xname = 'Parameters', (*self.Parameters).Name + '  (' + (*self.Parameters).Units + ')' $
		, 	Yname = 'Fitted Values', (*self.Parameters).Value
		return, 0B
	endif


	Menu = widget_info(widget_info(ev.top,/child),/all_children)
	for i=0L,n_elements(Menu)-1 do widget_control, Menu[i], /sensitive
	PMI__Control, ev.top, Viewer = 'PMI__DISPLAY__2DVIEW'
	PMI__Control, ev.top, /refresh
	return, 0B
END

FUNCTION PMI__Button__FitSingleInletRoi__Display__Event, ev

	widget_control, ev.handler, get_uvalue=self
	return, Self -> Event(ev)
END



FUNCTION PMI__Button__FitSingleInletRoi__Display::GetCurve, List

	PMI__Info, tlb(self.id), Stdy=Stdy
	Region = Stdy->Obj(1,widget_info(widget_info(self.id,find_by_uname=List),/droplist_select))
	Self -> GET, OnDisplay=OnDisplay, Time=Time
	Self -> SET, Message = 'Loading ' + Region->Name() + ' curve', Sensitive=0
	Y = PMI__RoiCurve(Stdy->DataPath(), Self.Series, Region, cnt=cnt)
	Self -> SET, OnDisplay=OnDisplay, /Sensitive
	if cnt eq 0 then return, Time*0
	case self.units of
	'Linear (a.u.)': relative=0
	'Linear (%)': relative=1
	'DSC-MRI': relative=2
	endcase
	return, LMU__Enhancement(Y,Self.Baseline,relative=relative)
END

FUNCTION PMI__Button__FitSingleInletRoi__Display::GetName, List

	PMI__Info, tlb(self.id), Stdy=Stdy
	Region = Stdy -> Obj(1,widget_info(widget_info(self.id,find_by_uname=List),/droplist_select))
	return, Region -> Name()
END


PRO PMI__Button__FitSingleInletRoi__Display::GET $
, 	CursorPos = CursorPos $
,	Model=Model, Positivity=Positivity, Delay=Delay $
,	Time=Time, Fit=Fit $
, 	RoiCurve=RoiCurve, AifCurve=AifCurve $
,	RoiName=RoiName, AifName=AifName $
, 	OnDisplay=OnDisplay

	if arg_present(CursorPos) then CursorPos=self.CursorPos

	if arg_present(Model) then begin
		list = widget_info(self.id,find_by_uname='FIT')
		widget_control, list, Get_Value=Models
		Model = Models[widget_info(list,/droplist_select)]
	endif
	if arg_present(Positivity) then $
	    Positivity = 0
	;	Positivity = widget_info(widget_info(self.id,find_by_uname='Positive'),/Button_set)
	if arg_present(Delay) then $
		Delay = widget_info(widget_info(self.id,find_by_uname='Delay'),/Button_set)

	if arg_present(Time) then begin
		t = Self.Series -> c(1)
		Time = t-t[0]
	endif

	if arg_present(RoiName) then RoiName=Self->GetName('ROI')
	if arg_present(AifName) then AifName=Self->GetName('AIF')

	if arg_present(RoiCurve) then $
		if ptr_valid(Self.Curve[0]) then RoiCurve = *Self.Curve[0] $
		else begin
			RoiCurve = Self->GetCurve('ROI')
			Self.Curve[0] = ptr_new(RoiCurve)
		endelse

	if arg_present(AifCurve) then $
		if ptr_valid(Self.Curve[1]) then AifCurve = *Self.Curve[1] $
		else begin
			AifCurve = Self->GetCurve('AIF')/(1-self.Hematocrit)
			Self.Curve[1] = ptr_new(AifCurve)
		endelse

	if arg_present(FIT) then begin
		if not ptr_valid(Self.Curve[2]) then Self->Fit
		FIT = *Self.Curve[2]
		endif

	if arg_present(OnDisplay) then begin
		OnDisplay = ''
		List = ['AIF','ROI','FIT']
		for i=0L,2 do $
		if 0 eq widget_info(widget_info(self.id,find_by_uname=List[i]+'bttn'),/sensitive) then OnDisplay=List[i]
	endif
END

PRO PMI__Button__FitSingleInletRoi__Display::SET, $
	PMI__REFRESH=pmi__refresh, PMI__RESIZE=pmi_resize, $
	Refresh=Refresh, Erase=Erase, $
	Message=Message, Sensitive=Sensitive, $
	xsize=xsize, ysize=ysize, $
	Set_droplist_select = Set_droplist_select, $
	Series=Series, Units=Units, Baseline=Baseline, Hematocrit=Hematocrit, $
	OnDisplay=OnDisplay

	if keyword_set(pmi_resize) then begin
		gpmi = widget_info(/geometry, tlb(self.id))
		self -> Set, xsize=gpmi.xsize, ysize=gpmi.ysize, /REFRESH
	endif


	if n_elements(Series) 	ne 0 then Self.Series = Series
	if n_elements(Units) 	ne 0 then self.Units = Units
	if n_elements(Baseline) 	ne 0 then self.Baseline = Baseline
	if n_elements(Hematocrit) 	ne 0 then self.Hematocrit = Hematocrit
	if n_elements(Message) 	ne 0 then begin
		Self -> SET, OnDisplay='', /Refresh
		xyouts, 0.1,0.9, Message, color=0,/normal,charsize=1.5,charthick=2.0
	endif
	if n_elements(Sensitive) ne 0 then widget_control, self.id, sensitive=sensitive
	if n_elements(Set_droplist_select) ne 0 then begin
		List = ['ROI','AIF']
		for i=0,1 do widget_control, $
			widget_info(self.id,find_by_uname=List[i]), $
			Set_droplist_select=Set_droplist_select[i]
	endif
	if n_elements(xsize) ne 0 $
	or n_elements(ysize) ne 0 then begin
		widget_control, self.DrawId, xsize=xsize, ysize=ysize-40
		xs = floor((xsize - 460)/3E)
		if xs lt 50 then xs = 50
		List = ['AIF','ROI','FIT']
		for i=0,2 do widget_control, widget_info(self.id,find_by_uname=List[i]), xsize=xs
	endif

	if keyword_set(Erase) then begin
		widget_control, self.DrawId, get_value = win
		wset, win & erase, 255
	endif

	if n_elements(OnDisplay) ne 0 then begin

		List = ['AIF','ROI','FIT']
		for i=0,2 do $
		widget_control, widget_info(self.id,find_by_uname=List[i]+'bttn'), Sensitive=List[i] ne OnDisplay
 		widget_control, widget_info(self.id,find_by_uname='Export')		, Sensitive=OnDisplay eq 'FIT'
		widget_control, widget_info(self.id,find_by_uname='Export As')	, Sensitive=OnDisplay eq 'FIT'
	endif

	if keyword_set(Refresh) then begin

		Menu = widget_info(widget_info(tlb(self.id),/child),/all_children)
		for i=0L,n_elements(Menu)-1 do widget_control, Menu[i], sensitive=0
		Self -> Plot
	endif
END





PRO PMI__Button__FitSingleInletRoi__Display::Cleanup
	widget_control, self.id, /destroy
	ptr_free, Self.Curve, self.Parameters
	loadct, 0
END

FUNCTION PMI__Button__FitSingleInletRoi__Display::Init, parent, CursorPos, xsize=xsize, ysize=ysize

	if n_elements(CursorPos) ne 0 then self.CursorPos = CursorPos

	loadct, 12

	PMI__Info, tlb(parent), Stdy=Stdy

	self.id = widget_base(parent,/column,map=0,event_func='PMI__Button__FitSingleInletRoi__Display__Event')
	Controls = widget_base(self.id,/row,ysize=40,/base_align_center,space=5)
	self.DrawId	= widget_draw(self.id,/retain)

		v = ['ROI','AIF']
		for i=0,1 do begin
			Base = widget_base(Controls,/row,/frame,/base_align_center)
			id = widget_button(Base, xsize=25, ysize=19, value=v[i], uname=v[i]+'bttn')
  			id = widget_droplist(Base,/dynamic_resize, value=Stdy->Names(1), uname=v[i])
  		endfor

        models = ['Maximum slope', 'Uptake', 'Steady State', 'Patlak','Model-free', 'Compartment','Tofts','Modified Tofts','Modified Tofts (Linear)','2C Uptake','2C Exchange']

		Base = widget_base(Controls,/row,/frame,/base_align_center)
			id = widget_button(Base, xsize=25, ysize=19, value='FIT', uname='FITbttn')
  			id = widget_droplist(Base,/dynamic_resize, uname='FIT',value = models)
            widget_control, id, set_droplist_select = 7

		v = ['Positive','Delay']
		for i=1,1 do begin
			Base = widget_base(Controls,/row,/frame,/nonexclusive)
			id = widget_button(Base, value=v[i], uname=v[i])
		endfor

		v = ['Export','Export As','Close']
		Base = widget_base(Controls,/row,/frame,/base_align_center)
		for i=0,2 do id = widget_button(Base, xsize=50, ysize=22, value=v[i], uname=v[i])

	self -> Set, xsize=xsize, ysize=ysize, OnDisplay='FIT'
	widget_control, self.id, set_uvalue = self, /map
	return, 1
END

PRO PMI__Button__FitSingleInletRoi__Display__Define

	Struct = {PMI__Button__FitSingleInletRoi__Display 	$
	,	id: 0L 	$
	,	DrawId: 0L $
	,	CursorPos:lonarr(4)	$
	,	Curve:ptrarr(3) $ ;RoiCurve, AifCurve, Fit
	,	Parameters: ptr_new() $
	,	Units: '' $
	,	Series: obj_new() $
	,	Baseline: 0L $
	,	Hematocrit: 0E $
	}
END





pro PMI__Button__Event__FitSingleInletRoi, ev

	PMI__Info, ev.top, Stdy=Stdy

    Series = Stdy->Names(0,ns,DefDim=3,ind=ind,sel=sel)
    Regions = Stdy->Names(1,nr)
    Units = ['Linear (a.u.)','Linear (%)','DSC-MRI']

	v = PMI__Form(ev.top, Title='Perfusion analysis setup', [$
		ptr_new({Type:'DROPLIST',Tag:'series', Label:'Dynamic series', Value:Series, Select:sel}), $
		ptr_new({Type:'DROPLIST',Tag:'aif'	 , Label:'Arterial Region', Value:Regions, Select:stdy->sel(1)}), $
		ptr_new({Type:'DROPLIST',Tag:'roi'	 , Label:'Tissue Region', Value:Regions, Select:stdy->sel(1)}), $
		ptr_new({Type:'DROPLIST',Tag:'units' , Label:'Signal model', Value:Units, Select:0}), $
		ptr_new({Type:'VALUE'	,Tag:'nbase' , Label:'Length of baseline (# of dynamics)', Value:1L}),$
		ptr_new({Type:'VALUE'	,Tag:'hct'	 , Label:'Patients hematocrit', Value:0.45})])
	IF v.cancel THEN return

	PMI__Control, ev.top, Viewer = 'PMI__Button__FitSingleInletRoi__Display', Display=Display

	Display -> Set, /Refresh, $
		Series = Stdy->Obj(0,ind[v.series]), $
		Units = Units[v.units], $
		Baseline = v.nbase, $
		Hematocrit = v.hct, $
		set_droplist_select = [v.roi,v.aif]
end

pro PMI__Button__Control__FitSingleInletRoi, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 1)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__FitSingleInletRoi, parent,value=value, separator=separator

	SingleInletUptake
	SingleInletSteadyState
	SingleInletPatlak
	SingleInletCompartment
	SingleInletModifiedTofts
	SingleInlet2CUptakeExchange
	SingleInletExchange

	if n_elements(value) eq 0 then value = 'Fit single-inlet exchange models (ROI)'

	id = widget_button(parent 	$
	,	value = value	$
	,	event_pro = 'PMI__Button__Event__FitSingleInletRoi'	$
	,	pro_set_value = 'PMI__Button__Control__FitSingleInletRoi' $
	, 	separator = separator )
	return, id
end
