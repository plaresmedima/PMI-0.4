pro PMI__Button__Event__ExportLiverInterobserver, ev

  PMI__Info, ev.top, State=State, status=status
  if not State->get_dir(path,title='Where to save results?') then return
  PMI__Control, ev.top, Viewer = 'PMI__Display__PerfusionRoiOutput', Display=Display

  Observer = ['WS','SS']
  ROIs = ['NALT1', 'NALT2', 'NALT3', 'NALT4', 'NALT5', 'Lesion']
  Output = ['Study', 'ROI', 'Observer', 'Parameter', 'Value']
  nrois = 0L

  ;Loop over all studies
  for k=0L, State->n()-1 do begin
    Stdy = state->obj(k)
    Series = Stdy->Names(0,ns)

    ;Find the dynamic series and read the baseline
    ;Dynamic series with baseline 9 must be named 'xTWIST(9)',
    i = -1
    for j=0L,ns-1 do begin
    	name = Series[j]
    	if strmid(name,0,6) eq 'xTWIST' then begin
    		i=j
    		baseline = long(strmid(name,7,strlen(name)-8))
    	    TWIST = Stdy->Obj(0,i)
        	Time = TWIST->c(1)
      		Time = Time-Time[0]
    	endif
    endfor

    if i ge 0 then begin
    	Regions = Stdy->Names(1,nr)
    	;for each observer, fit all ROI data
    	;Input functions for observer WS must be named 'AIF_WS', 'VIF_WS'
    	for o=0,1 do begin
    		AIF = Where(Regions EQ 'AIF'+'_'+Observer[o], na)
    		VIF = Where(Regions EQ 'VIF'+'_'+Observer[o], nv)
    		if na+nv EQ 2 then begin
      			AIF = LMU__Enhancement(PMI__RoiCurve(Stdy->DataPath(), TWIST, Stdy->Obj(1,AIF), status),baseline,/relative)/0.55
      			VIF = LMU__Enhancement(PMI__RoiCurve(Stdy->DataPath(), TWIST, Stdy->Obj(1,VIF), status),baseline,/relative)/0.55
      			;Loop over all ROIs
      			;Input functions for observer WS must be named 'NALT1_WS', 'NALT2_WS', ..., 'Lesion_WS'
				for r=0L,5 do begin
					ROI = Where(Regions EQ ROIs[r]+'_'+Observer[o], cnt)
					if cnt gt 0 then begin
						;Fit the ROI curve
						ROI = LMU__Enhancement(PMI__RoiCurve(Stdy->DataPath(), TWIST, Stdy->Obj(1,ROI), status),baseline,/relative)
						P = [0.2*50/6000D,0.8*50/6000D,0.2,0.2] ;P = [FA,FV,VE,UF]
      					FIT = FitDualInlet('Uptake', Time, AIF, VIF, ROI, P, DELAY_PAR=Pd, DELAY_VALUES=[0,20,1], DELAY_WHICH=0, LIMITED_ABOVE=[0,0,1,1], /POSITIVITY, /NODERIVATIVE)
        				Parameters = $
          					[{Name:'Extracellular Volume' 	,Units:'ml/100ml'   	,Value:100D*P[2]   						,Nr:0,Rnd:1 } $
         					,{Name:'Extracellular MTT'    	,Units:'sec'      		,Value:1D*P[2]*(1-P[3])/(P[0]+P[1]) 	,Nr:1,Rnd:1 } $
          					,{Name:'Uptake Fraction'    	,Units:'%'        		,Value:100D*P[3] 						,Nr:2,Rnd:1 } $
          					,{Name:'Uptake Rate'      		,Units:'/100/min'   	,Value:6000D*(P[0]+P[1])*P[3]/(1-P[3])	,Nr:3,Rnd:1 } $
          					,{Name:'Arterial Plasma Flow' 	,Units:'ml/min/100ml' 	,Value:6000D*P[0]  						,Nr:4,Rnd:1 } $
          					,{Name:'Venous Plasma Flow'   	,Units:'ml/min/100ml' 	,Value:6000D*P[1] 						,Nr:5,Rnd:1 } $
          					,{Name:'Total Plasma Flow'   	,Units:'ml/min/100ml' 	,Value:6000D*(P[0]+P[1])				,Nr:6,Rnd:1 } $
          					,{Name:'Arterial Flow Fraction' ,Units:'%'        		,Value:100*P[0]/(P[0]+P[1])				,Nr:7,Rnd:1 } $
          					,{Name:'Arterial Delay' 		,Units:'sec'			,Value:1D*Pd							,Nr:8,Rnd:1 } ]
        				;Save output
        				Name  = Stdy->Name() + '_' + ROIs[r] + '_' + Observer[o]
        				Display -> Set, /Refresh $
          					, Model = 'Dual-inlet uptake' $
          					, Time = Time $
          					, Curve = ROI $
          					, Fit = FIT $
          					, Units = 'Relative Signal Enhancement' $
          					, RoiName = Name $
          					, Parameters = Parameters
          			   Write_tiff, Path + name + '.tif', reverse(tvrd(/true),3)
          			   nrois = nrois+1
          			   for p=0L,8 do Output = [Output, Stdy->Name(), ROIs[r], Observer[o], Parameters[p].name, strcompress(Parameters[p].value,/remove_all)]
					endif
				endfor
    		endif
    	 endfor
      endif
	endfor

	PMI__Write_CSV, Path + 'interobserver.csv', reform(output,5,1+nrois*9)
end



pro PMI__Button__Control__ExportLiverInterobserver, id, v

  PMI__Info, tlb(id), Stdy=Stdy
  if obj_valid(Stdy) then begin
    Series = Stdy->Names(0,ns,DefDim=3)
    Regions = Stdy->Names(1,nr)
    sensitive = (ns gt 0) and (nr gt 2)
  endif else sensitive=0
  widget_control, id, sensitive=sensitive
end



function PMI__Button__ExportLiverInterobserver, parent,value=value, separator=separator

  ok = FitDualInletUptake()
  PMI__Display__PerfusionRoiOutput__Define

  if n_elements(value) eq 0 then value = 'Export Interobserver Liver'

  id = widget_button(parent $
    ,	value 		= value $
    ,	event_pro 	= 'PMI__Button__Event__ExportLiverInterobserver'$
    ,	pro_set_value 	= 'PMI__Button__Control__ExportLiverInterobserver' $
    , 	separator 	= separator	)

  return, id

end
