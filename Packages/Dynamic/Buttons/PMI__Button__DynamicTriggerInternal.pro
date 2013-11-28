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
FUNCTION PMI__Button__Input__DynamicTriggerInternal, top,series,indices,v

	PMI__Info, top, status=status, Stdy=Stdy
   	AllSeries = Stdy->Names(0,ns,DefDim=3,ind=ind,sel=sel)
    AllRegions = Stdy->names(1,nr)
	v = {series:sel, trigger:Stdy->sel(1), minima:1, interp:1, tol:100E}

	WHILE 1 DO BEGIN
		v = PMI__Form(top, Title='Triggering setup', [$
			ptr_new({Type:'DROPLIST', Tag:'series'	, Label:'Dynamic series', Value:AllSeries, Select:v.series}), $
			ptr_new({Type:'DROPLIST', Tag:'trigger'	, Label:'Triggering Region', Value:AllRegions, Select:v.trigger}), $
			ptr_new({Type:'DROPLIST', Tag:'minima'	, Label:'Trigger on', Value:['Signal maxima','Signal minima'], Select:v.minima}), $
			ptr_new({Type:'DROPLIST', Tag:'interp'	, Label:'Interpolate?', Value:['No','Yes'], Select:v.interp}), $
			ptr_new({Type:'VALUE'	, Tag:'tol', Label:'Tolerance (%)', Value:v.tol})])
		IF v.cancel THEN return, 0
		Series = Stdy->Obj(0,ind[v.series])
		Region = Stdy->Obj(1,v.trigger)
		curve = PMI__RoiCurve(Stdy->DataPath(), Series, Region, X=t, status, cnt=cnt)
		IF cnt GT 0 THEN BEGIN
			indices = RetrospectiveTriggeringIndices(curve, t[1]-t[0], 1.0/15.0, v.tol, minima=v.minima, cnt=cnt)
 			IF cnt EQ 0 THEN BEGIN
 				msg = [ 'No maxima/minima in the triggering curve', $
 						'Please select another region' ]
				IF 'Cancel' EQ dialog_message(msg,/information,/cancel) THEN return, 0
			ENDIF ELSE return, 1
		ENDIF ELSE BEGIN
			msg = [	'Region '+Region->Name()+' is not defined on series '+Series->Name(), $
				'Please select another Region and/or Series']
    		IF 'Cancel' EQ dialog_message(msg,/information,/cancel) THEN return, 0
    	ENDELSE
	ENDWHILE
END

pro PMI__Button__Event__DynamicTriggerInternal, ev

	PMI__Info, ev.top, status=status, Stdy=Stdy

	if not PMI__Button__Input__DynamicTriggerInternal(ev.top,series,ind,v) then return

	New = Stdy->New('SERIES', $
		Default = Series, $
		Name = Series->Name() + '[Trig]' )

	Time = Series->c(1)

	IF NOT v.interp THEN BEGIN

		New -> t, time[ind]
		d = New->d()

		FOR j=0L,d[3]-1 DO BEGIN
			PMI__Message, status,  'Triggering', j/(d[3]-1E)
			im = Series -> Read(Stdy->DataPath(), -1, ind[j])
			New -> Write, Stdy->DataPath(), im, -1 ,j
		ENDFOR

	ENDIF ELSE BEGIN

		int = where( (time ge time[ind[0]]) and (time le time[ind[n-1]]), nt)
		New->t, time[int]
		d = Series->d()

		FOR i=0L,d[2]-1 DO BEGIN

			PMI__Message, status, 'Triggering', i/(d[2]-1E)
			im = Series->Read(Stdy->DataPath(),i,-1)
			im = reform(im,d[0]*d[1],d[3],/overwrite)
			FOR k=0L,d[0]*d[1]-1 DO BEGIN
				y = reform(im[k,*],/overwrite)
				im[k,0:nt-1] = interpol(y[ind],time[ind],time[int])
			ENDFOR
			New->Write, Stdy->DataPath(), im, i ,-1
		ENDFOR

	ENDELSE
	PMI__Control, ev.top, /refresh
end


pro PMI__Button__Control__DynamicTriggerInternal, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->names(1,nr)
		sensitive = (ns gt 0) and (nr gt 0)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end


function PMI__Button__DynamicTriggerInternal,$
	parent,$
	separator = separator

    return, widget_button(parent,$
    	separator = separator,$
    	value = 'Triggering',$
		event_pro = 'PMI__Button__Event__DynamicTriggerInternal',$
		pro_set_value = 'PMI__Button__Control__DynamicTriggerInternal')
end