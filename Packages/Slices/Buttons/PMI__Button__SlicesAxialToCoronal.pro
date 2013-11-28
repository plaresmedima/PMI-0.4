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
pro PMI__Button__Event__SlicesAxialToCoronal, ev

	; z <-> y

	PMI__Info, ev.top, Series=Series, Stdy=Stdy, Status=status

	d = Series->d() ;=[nx,ny,nz,nt]

	Rec = Stdy -> New('SERIES', $
		Default = Series, $
		name = Series->name()+'[Rec]', $
		Domain = {z:findgen(d[1]), t:Series->t(), m:[d[0],d[2]]} )

	for j=0L,d[3]-1 do begin ;j: zeit
		PMI__Message, status, 'Calculating Reconstruction ', j/(d[3]-1.0)
		vol = Series->Read(Stdy->DataPath(),-1,j)
		vol = transpose(vol,[0,2,1])
		vol = reverse(vol,3,/overwrite)
		Rec -> Write, Stdy->DataPath(), vol,-1,j
	endfor
	PMI__Control, ev.top, /refresh
end

pro PMI__Button__Control__SlicesAxialToCoronal, id, v

	PMI__Info, tlb(id), Series=Series
	sensitive = 0
	if obj_valid(Series) then sensitive = Series->d(2) gt 1
    widget_control, id, sensitive=sensitive
end

function PMI__Button__SlicesAxialToCoronal, $
	parent,$
	separator=separator,$
	value=value

	if n_elements(value) eq 0 then value = 'Reconstruction: Axial -> Coronal'

	RETURN, widget_button(parent, $
		value = value, 	$
		separator = separator, $
		event_pro = 'PMI__Button__Event__SlicesAxialToCoronal', $
		pro_set_value='PMI__Button__Control__SlicesAxialToCoronal')
end
