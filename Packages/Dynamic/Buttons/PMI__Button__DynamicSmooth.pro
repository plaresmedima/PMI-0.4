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
pro PMI__Button__Event__DynamicSmooth, ev

	PMI__Info, ev.top, status=status, Series=Series, Stdy=Stdy

	v = PMI__Form(ev.top, Title='Signal smoothing', $
		[ ptr_new({Type:'VALUE'	,Tag:'win',Label: 'Smoothing window', Value:3L}) ] $
		) & if v.cancel then return

	win = 2*(v.win/2) + 1
	half = (win-1)/2L
 	d = Series->d()

	New = Stdy->New('SERIES', $
	 	Name = Series->name()+'[Smth ' + strcompress(win,/remove_all) + ']', $
	 	Default	= Series )

	FOR i=0L,d[2]-1 DO BEGIN

		PMI__Message, Status, 'Calculating', i/(d[2]-1E)

		im = Series->Read(Stdy->DataPath(), i,-1)

		FOR k=0L,half-1 		DO im[*,*,k]=total(im[*,*,0:k+half],3)/(k+1+half)
		FOR k=half,d[3]-1-half 	DO im[*,*,k]=total(im[*,*,k-half:k+half],3)/win
		FOR k=d[3]-half,d[3]-1 	DO im[*,*,k]=total(im[*,*,k-half:*],3)/(d[3]-k+half)

		New -> Write, Stdy->DataPath(), im, i, -1
	ENDFOR

	PMI__Control, ev.top, /refresh
end


pro PMI__Button__Control__DynamicSmooth, id, v

	PMI__Info, tlb(id), Series=Series
	sensitive = 0
	if obj_valid(Series) then sensitive = Series->d(3) gt 1
    widget_control, id, sensitive=sensitive
end

function PMI__Button__DynamicSmooth $
, 	parent $
, 	separator = separator $
,	value=value

	if n_elements(value) eq 0 then value='Smooth'

	return, widget_button(parent, $
		separator	= separator, $
	 	value 		= value, $
	 	event_pro 	= 'PMI__Button__Event__DynamicSmooth', $
	  	pro_set_value= 'PMI__Button__Control__DynamicSmooth')
end
