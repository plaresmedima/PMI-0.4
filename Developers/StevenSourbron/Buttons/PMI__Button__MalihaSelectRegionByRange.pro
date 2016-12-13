pro PMI__Button__MalihaSelectRegionByRange__event, ev

	PMI__info, ev.top, Stdy=Stdy, status=status, Display=Display

	sel = stdy->sel(0)
	if sel eq -1 then sel=0

 	in = cw_inputform(/pos,ev=ev, $
 		Labels = $
 		[	'Parameter: ' $
 		,	'Maximum value:' $
 		, 	'Minimum value:' $
 		] $
 	,  ListNames  = [ Stdy->Names(0)]$
    ,  ListDefaults = [ sel]$
 	,  DataDefaults = {v1:1000E, v0:100E} $
	) & if size(in,/type) eq 1 then return

	Series = Stdy -> Obj(0,in.select[0])
	vMin = in.data.v0
	vMax = in.data.v1

	Roi = Stdy->New('REGION' $
	,	Name = 'Whole Cortex' $
	,	Domain = Series->dom() $
	,	Color = Series->Clr(SAT='R'))

	d = Series->d()
	bin = bytarr(d[0]*d[1])

	Display->GET, CursorPos=p
 	Im = Series->Read(Stdy->DataPath(),p[2],0)
 	ind = where( (Im gt vMin) and (Im lt vMax), cnt)
 	if cnt gt 0 then begin
 		bin[ind] = 1
 		Roi -> Write, Stdy->DataPath(), bin, p[2],0
 		bin[ind] = 0
 	endif

	PMI__Control, ev.top, /refresh
end

pro PMI__Button__Control__MalihaSelectRegionByRange, id, v

	PMI__Info, tlb(id), Series=Series
	widget_control, id, sensitive = obj_valid(Series)
end


function PMI__Button__MalihaSelectRegionByRange, parent, separator=separator

  	id = widget_button(parent					$
  	, 	separator	= separator 				$
  	, 	value 		= 'Select region by range'			$
  	,	pro_set_value = 'PMI__Button__Control__MalihaSelectRegionByRange' $
	, 	event_pro	= 'PMI__Button__MalihaSelectRegionByRange__event'	)

	return, id
end