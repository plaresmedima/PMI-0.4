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

FUNCTION PMI__Button__Input__PKmotionCorrection, top, series, aif, in

    PMI__Info, top, Stdy=Stdy
    DynSeries = Stdy->Names(0,DefDim=3,ind=ind,sel=sel)
	in = {ser:sel, aif:stdy->sel(1), nb:1}

	WHILE 1 DO BEGIN

		in = PMI__Form(top, Title='Motion correction setup', [$
		ptr_new({Type:'DROPLIST',Tag:'ser', Label:'Dynamic series', Value:DynSeries, Select:in.ser}), $
		ptr_new({Type:'DROPLIST',Tag:'aif', Label:'Arterial Region', Value:Stdy->names(1), Select:in.aif}), $
		ptr_new({Type:'VALUE'	,Tag:'nb' , Label:'Length of baseline (# of dynamics)', Value:in.nb})])
		IF in.cancel THEN return, 0

    	Series = Stdy->Obj(0,ind[in.ser])
    	IF (in.nb LT 1) or (in.nb GT Series->d(3)) THEN BEGIN
    		in.nb = 10
    		msg = ['Baseline length must be less than the total number of dynamics',$
    		'Please select another baseline length']
    	ENDIF ELSE BEGIN
	   		Aif = PMI__RoiCurve(Stdy->DataPath(), Series, Stdy->Obj(1,in.aif), status, cnt=cnt)
    		IF cnt EQ 0 THEN $
    			msg = ['Arterial region is empty on this series',$
    			'Please select another region and/or series'] $
    		ELSE IF n_elements(Aif) NE Series->d(3) THEN $
    			msg = ['Arterial region is not defined on every dynamic',$
    			'Please select another region and/or series'] $
    		ELSE BEGIN
    			Aif = LMU__Enhancement(Aif,in.nb,relative=0)/(1-0.45)
				return, 1
    		ENDELSE
    	ENDELSE
     	IF 'Cancel' EQ dialog_message(msg,/information,/cancel) THEN return, 0
  	ENDWHILE
END



pro PMI__Button__Event__PKmotionCorrection, ev

	PMI__Info, ev.top, Status=Status, Stdy=Stdy
	PMI__Message, status, 'Preparing calculation..'

    IF NOT PMI__Button__Input__PKmotionCorrection(ev.top,series,aif,in) THEN RETURN

	PMI__Message, status, 'Preparing calculation..'

	pixel_spacing = Series -> GetValue('0028'x,'0030'x)
	slice_gap = Series -> GetValue('0018'x,'0088'x)
	if pixel_spacing eq 0 then begin
	  tmp = PMI__Form(ev.top, Title='Please check defaults', [$
		ptr_new({Type:'VALUE'	,Tag:'ps' , Label:'Pixel spacing', Value:3.125}), $
		ptr_new({Type:'VALUE'	,Tag:'sg' , Label:'Slice gap', Value:4.0})])
		IF tmp.cancel THEN begin
		  PMI__Control, ev.top, /refresh
		  return
		endif
		pixel_spacing = tmp.ps
		slice_gap = tmp.sg
	endif
	voxel_sizes = [pixel_spacing, pixel_spacing, slice_gap]
    matrix = Series->m()
    d = Series->d()
	time = Series->t() - Series->t(0)
	Source = Series->Read(Stdy->DataPath())

    PMI__Message, status, 'Calculating..'

    SourceIso = resample_isotropic(Source, voxel_sizes, matrix)
    DeformedIso = pkreg_mres(Time, aif, SourceIso, in.nb)
    Deformed = resample_volumes(DeformedIso, d)
    Dom = {z:Series->z(), t:Series->t(), m:Series->m()}
    Corr = Stdy->New('SERIES', Domain=Dom,  Name='Motion-free' )
	Corr -> Write, Stdy->DataPath(), Deformed
	Corr -> Trim, Series->Trim()

    PMI__Control, ev.top, /refresh
end


pro PMI__Button__Control__PKmotionCorrection, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 0)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__PKmotionCorrection, parent,value=value,separator=separator

    if n_elements(value) eq 0 then value = 'PK motion correction'

    id = widget_button(parent $
    ,   value = value  $
    ,  	event_pro = 'PMI__Button__Event__PKmotionCorrection' $
    ,	pro_set_value = 'PMI__Button__Control__PKmotionCorrection' $
    ,  	separator = separator )

    return, id
end

