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


pro PMI__Button__Event__ViewRoiCurve, ev

    PMI__info, ev.top, Series=S, Region=R, Stdy=Stdy, Status=id

    if S->d(3) eq 1 then begin
    	S = Stdy->Names(0,n,DefDim=3,ind=ind)
	   	if n gt 1 then begin
    		in = cw_InputForm(/pos, ev=ev, Labels='Dynamic series',ListNames=S)
			if size(in,/type) eq 1 then return
			i = in.select[0]
		endif else i = 0L
		S = Stdy->Obj(0,ind[i])
    endif

    Curve = PMI__RoiCurve(Stdy->DataPath(),S,R,id,cnt=cnt,X=X)

    if cnt eq 0 then begin
    	ok = dialog_message(/information,'Region is not defined on this series')
    	return
    endif

	Display = PMI__DisplayNew(ev.top,'PMI__Display__ViewRoiCurve')

	Display -> SET, /Refresh $
	, 	Xaxis=X-X[0], Yaxis=Curve $
	, 	Title=R->Name(), Xtitle='Time (s)', Ytitle=S->Name()

end




pro PMI__Button__Control__ViewRoiCurve, id, v

	PMI__Info, tlb(id), Stdy=Stdy, Series=Series, Region=Region, Viewer=Viewer
	sensitive = (Viewer ne 'PMI__DISPLAY__VIEWROICURVE') and obj_valid(Series) and obj_valid(Region)
	if sensitive then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		nr = Stdy->n(1)
		sensitive = (ns gt 0) and (nr gt 0)
	endif
    widget_control, id, sensitive=sensitive
end

function PMI__Button__ViewRoiCurve, parent, separator=Separator

	PMI__Display__ViewRoiCurve__Define

    id = widget_button(parent   $
    ,	value  = 'ROI curve'  $
    ,  	event_pro  = 'PMI__Button__Event__ViewRoiCurve'  $
   	,	pro_set_value = 'PMI__Button__Control__ViewRoiCurve' $
    ,  	separator = Separator  $
    )
    return, id
end
