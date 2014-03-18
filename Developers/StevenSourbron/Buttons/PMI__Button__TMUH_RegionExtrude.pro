pro PMI__Button__Event__TMUH_RegionExtrude, ev

	PMI__info $
	, 	ev.top $
	, 	Stdy = Stdy $
	, 	Status = Status $
	, 	Region = Region $
	, 	Series = Series $
	, 	Display = Display

	dim = 2+1
	dir = 2
	Display	-> Get, CursorPos=p

	Extrude = Stdy->New('REGION' $
	,	Name 	= Region->name()+'[Extrude]' $
	,	Default	= Region $
	,	Domain  = Series->Dom() )

	Path = Stdy->DataPath()

	dR = Region	-> d()
	dS = Series	-> d()

	case dir of
		0: r=[p[dim],dS[dim]-1]
		1: r=[0L,p[dim]]
		2: r=[0L,dS[dim]-1]
	endcase

	nr = r[1]-r[0]+1

	if dim eq 2 then begin

		for j=0L,dS[3]-1 do begin

			PMI__Message, status, 'Extruding Region ' + Region->Name(), j/(dS[3]-1E)

			Bin = 0B
		;	for i=0L,dR[2]-1 do Bin = Bin or Region->Read(Path,z=Region->z(i),t=Series->t(j))
			for i=0L,dR[2]-1 do Bin = Bin or Region->Read(Path,i,j)
			if total(Bin) gt 0 then for i=r[0],r[1] do Extrude->Write, Path, bin, i, j
		endfor
	endif

	if dim eq 3 then begin

		for i=0L,dS[2]-1 do begin

			PMI__Message, status, 'Extruding Region ' + Region->Name(), i/(dS[2]-1E)

			Bin = 0B
		;	for j=0L,dR[3]-1 do Bin = Bin or Region->Read(Path,z=Series->z(i),t=Region->t(j))
			for j=0L,dR[3]-1 do Bin = Bin or Region->Read(Path,i,j)
			if total(Bin) gt 0 then for j=r[0],r[1] do Extrude->Write, Path, bin, i, j
		endfor
	endif

	PMI__control, ev.top, /refresh
end

pro PMI__Button__Control__TMUH_RegionExtrude, id, v

	PMI__Info, tlb(id), Region=Region
	widget_control, id, sensitive = obj_valid(Region)
end

function PMI__Button__TMUH_RegionExtrude	$
, 	parent $
, 	separator = separator, value=value

	if n_elements(value) eq 0 then value='Extrude'

  	id = widget_button(parent $
  	, 	separator	= separator $
  	, 	value 		= value $
	, 	event_pro	= 'PMI__Button__Event__TMUH_RegionExtrude' $
	,	pro_set_value = 'PMI__Button__Control__TMUH_RegionExtrude')

	return, id
end