;    Copyright (C) 2013 Steven Sourbron
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
;    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.



pro PMI__Button__Event__RegionExtrude, ev

	PMI__info $
	, 	ev.top $
	, 	Stdy = Stdy $
	, 	Status = Status $
	, 	Region = Region $
	, 	Series = Series $
	, 	Display = Display

	in = PMI__Form(ev.top, Title='Extrude region..', [$
	ptr_new({Type:'DROPLIST', Label:'Extrude in which dimension?', select:1, Value:['Slices', 'Dynamics']}), $
	ptr_new({Type:'DROPLIST', Label:'Extrude in which direction?', select:2, Value:['Forwards','Backwards','Both'] })])
	IF in.cancel THEN return

	dim = 2+in.(0)
	dir = in.(1)
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

pro PMI__Button__Control__RegionExtrude, id, v

	PMI__Info, tlb(id), Region=Region
	widget_control, id, sensitive = obj_valid(Region)
end

function PMI__Button__RegionExtrude	$
, 	parent $
, 	separator = separator

  	id = widget_button(parent $
  	, 	separator	= separator $
  	, 	value 		= 'Extrude..' $
	, 	event_pro	= 'PMI__Button__Event__RegionExtrude' $
	,	pro_set_value = 'PMI__Button__Control__RegionExtrude')

	return, id
end