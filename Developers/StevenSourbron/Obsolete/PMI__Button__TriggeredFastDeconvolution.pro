pro PMI__Button__TriggeredFastDeconvolution__Calculate $
,	Stdy $
,	DceSeries $
,	StatusId $
,	pCurves,pInd,AIF,Time

	d = DceSeries->d()
	PF = fltarr(mult(d[0:2])) & VD=PF & TT=PF
	n = n_elements(pCurves[*,0])

	MAT = (Time[1]-Time[0])*convolution_matrix(AIF)
	InvertIllPosedFast, pCurves, MAT, REGPAR = 0.1, Id = StatusId

	PMI__Message, StatusId, 'Calculating parameters'

	PF[pInd] = 6000*max(pCurves,dimension=2)
	VD[pInd] = 100*(Time[1]-Time[0])*total(pCurves,2)
	TT[pInd] = 60*remove_inf(VD[pInd]/PF[pInd])

	PMI__Message, StatusId, 'Saving Maps'

	Dom = {z:DceSeries->z(), t:DceSeries->t(0), m:DceSeries->m()}

	O=Stdy->New('SERIES',Data=PF, Domain=Dom, ClrWin=[0E,500E], Name='Plasma Flow (ml/100ml/min)')
	O=Stdy->New('SERIES',Data=VD, Domain=Dom, ClrWin=[0E,120E], Name='Volume of Distribution (ml/100ml)')
	O=Stdy->New('SERIES',Data=TT, Domain=Dom, ClrWin=[0E,180E], Name='Mean Transit Time (sec)')
end

function PMI__Button__TriggeredFastDeconvolution__PixelCurves $
, 	Path $
, 	DceSeries $
, 	Roi $
,	Bat $
,	Ind $
, 	StatusId $
,	Time = Time $
,	Aif = Aif $
, 	pInd = pInd $
, 	pCurves = pInt

	;GET INDICES OF ROI PIXELS

    j=0L
    pInd = Roi -> Where(Path,t=Roi->t(j),n=n)
    while n eq 0 do begin
    	j=j+1
    	pInd = Roi -> Where(Path,t=Roi->t(j),n=n)
    endwhile


	Time = DceSeries -> t()
	nb = total(Time[ind] le Bat)
	if nb eq 0 then nb=1


	ni = 1 + ind[n_elements(ind)-1] - ind[0]
	i = ind[0] + lindgen(ni)



	p = PMI__PixelCurve(Path,DceSeries,Roi,StatusId,cnt=np)

	pInt = fltarr(np,ni)


	for k=0L,np-1 do begin

		PMI__Message, StatusId, 'Interpolating pixel curves', k/(np-1E)

		c = reform(p[k,ind])
		c0 = total(c[0:nb-1])/nb
		c = c-c0
		pInt[k,*] = o1_interpol(Time[ind],c,Time[i])
	endfor

	Time = Time[i]
	AIF = AIF[i]

	return, 1
end

pro PMI__Button__Event__TriggeredFastDeconvolution, ev

	PMI__Info, ev.top, Status=StatusId, Stdy=Stdy
    DynSeries = Stdy->Names(0,DefDim=3,ind=ind,sel=sel)

	in = PMI__Form(ev.top, Title='Deconvolution analysis with triggering', [$
	ptr_new({Type:'DROPLIST',Tag:'ser', Label:'Dynamic series', Value:DynSeries, Select:sel}), $
	ptr_new({Type:'DROPLIST',Tag:'aif', Label:'Arterial Region', Value:Stdy->names(1), Select:stdy->sel(1)}), $
	ptr_new({Type:'DROPLIST',Tag:'roi', Label:'Tissue Region', Value:Stdy->names(1), Select:stdy->sel(1)}), $
	ptr_new({Type:'DROPLIST',Tag:'trig', Label:'Triggering Region', Value:Stdy->names(1), Select:stdy->sel(1)}), $
	ptr_new({Type:'VALUE'	,Tag:'nb' , Label:'Length of baseline (# of dynamics)', Value:10L}),$
	ptr_new({Type:'VALUE'	,Tag:'hct', Label:'Patients hematocrit', Value:0.45}),$
	ptr_new({Type:'DROPLIST',Tag:'on', Label:'Trigger on..', Value:['Minima','Maxima'], Select:1})])
	IF in.cancel THEN goto, return

	DceSeries = Stdy->Obj(0,ind[in.ser])

	Aif = PMI__RoiCurve(Stdy->DataPath(),DceSeries,Stdy->Obj(1,in.aif),StatusId)
	Trig = PMI__RoiCurve(Stdy->DataPath(),DceSeries,Stdy->Obj(1,in.trig),StatusId,X=Time)

	Aif = LMU__Enhancement(Aif,in.nb,relative=0)/(1-in.hct)
	Ind = RetrospectiveTriggeringIndices(Trig, Time[1]-Time[0], 1.0/20.0, 100.0, minima=in.on EQ 0, cnt=cnt)

	BAT	= DceSeries->t(in.nb-1)
	ok = PMI__Button__TriggeredFastDeconvolution__PixelCurves(Stdy->DataPath(),DceSeries,Stdy->Obj(1,in.roi),Bat,Ind,StatusId,pCurves=p,pInd=pi,AIF=AIF,Time=Time)
	if not ok then goto, return

	PMI__Button__TriggeredFastDeconvolution__Calculate,Stdy,DceSeries,StatusId,p,pi,AIF,Time

	return: PMI__Control, ev.top, /refresh
end

pro PMI__Button__Control__TriggeredFastDeconvolution, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 1)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end


function PMI__Button__TriggeredFastDeconvolution, parent, separator=separator, value=value

	if n_elements(value) eq 0 then value = 'Fast deconvolution with triggering (Pixel)'

	return, widget_button(parent, $
		value = value,	$
		event_pro = 'PMI__Button__Event__TriggeredFastDeconvolution', $
		pro_set_value = 'PMI__Button__Control__TriggeredFastDeconvolution', $
		separator = separator	)
end
