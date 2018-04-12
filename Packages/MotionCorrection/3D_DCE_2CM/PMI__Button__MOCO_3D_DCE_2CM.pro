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

FUNCTION PMI__Button__Input__MOCO_3D_DCE_2CM, top, series, aif, in, Win

    PMI__Info, top, Stdy=Stdy
    DynSeries = Stdy->Names(0,DefDim=3,ind=ind,sel=sel)
	in = {ser:sel, aif:stdy->sel(1), roi:0L, res:16E, prec:1E, nb:1}

	WHILE 1 DO BEGIN

		in = PMI__Form(top, Title='Motion correction setup', [$
		ptr_new({Type:'DROPLIST',Tag:'ser', Label:'Dynamic series', Value:DynSeries, Select:in.ser}), $
		ptr_new({Type:'DROPLIST',Tag:'aif', Label:'Arterial Region', Value:Stdy->names(1), Select:in.aif}), $
		ptr_new({Type:'VALUE'	,Tag:'nb' , Label:'Length of baseline (# of dynamics)', Value:in.nb}), $
		ptr_new({Type:'DROPLIST',Tag:'roi', Label:'Region of Interest', Value:['<ENTIRE FOV>',Stdy->names(1)], Select:in.roi}), $
		ptr_new({Type:'VALUE'	,Tag:'res', Label:'Deformation Field Resolution (pixel sizes)', Value:in.res}), $
		ptr_new({Type:'VALUE'	,Tag:'prec', Label:'Deformation Field Precision (pixel sizes)', Value:in.prec}) $
		])
		IF in.cancel THEN return, 0

    	IF in.res LE 0 THEN BEGIN
    	  in.res = 16E
    	  msg = 'Deformation Field Resolution must be > 0'
    	  goto, jump
    	ENDIF
    	IF in.prec LE 0 THEN BEGIN
    	  in.prec = 1E
    	  msg = 'Deformation Field Precision must be > 0'
    	  goto, jump
    	ENDIF

    	Series = Stdy->Obj(0,ind[in.ser])
    	d = Series->d()
    	Win = {p:[0L,0L,0L], n:d[0:2]}
    	IF in.roi GT 0 THEN BEGIN
    	  Region = Stdy->Obj(1,in.roi-1)
    	  dim = Region->d()
    	  if dim[3] gt 1 then begin
    	    msg = 'Region of interest must be drawn on a static image'
    	    goto, jump
    	  endif
    	  Indices = Region->Where(Stdy->DataPath(), n=cnt)
    	  if cnt eq 0 then begin
    	    msg = 'Region is not defined on this series'
    	    goto, jump
    	  endif
    	  Pos = ARRAY_INDICES(dim[0:2], Indices, /DIMENSIONS)
    	  Win.p = [$
    	    min(Pos[0,*]),$
    	    min(Pos[1,*]),$
    	    min(Pos[2,*])]
    	  Win.n = [$
    	    1+max(Pos[0,*])-min(Pos[0,*]),$
    	    1+max(Pos[1,*])-min(Pos[1,*]),$
    	    1+max(Pos[2,*])-min(Pos[2,*])]
    	  if min(Win.n) le 1 then begin
    	    msg = 'Region of interest must cover more than 1 slice'
    	    goto, jump
    	  endif
    	ENDIF
    	IF in.nb LT 1 THEN BEGIN
    	  in.nb = 10
    	  msg = ['Baseline length must be > 1',$
    		  'Please select another baseline length']
    	  goto, jump
    	ENDIF
    	IF in.nb GT d[3] THEN BEGIN
    	  in.nb = 10
    	  msg = ['Baseline length must be less than the total number of dynamics',$
    		  'Please select another baseline length']
    	  goto, jump
    	ENDIF
	   	Aif = PMI__RoiCurve(Stdy->DataPath(), Series, Stdy->Obj(1,in.aif), status, cnt=cnt)
    	IF cnt EQ 0 THEN BEGIN
    	  msg = ['Arterial region is empty on this series',$
    			'Please select another region and/or series']
    	  goto, jump
    	ENDIF
    	IF n_elements(Aif) NE d[3] THEN BEGIN
    	  msg = ['Arterial region is not defined on every dynamic',$
    			'Please select another region and/or series']
    	  goto, jump
    	ENDIF
    	Aif = LMU__Enhancement(Aif,in.nb,relative=0)/(1-0.45)
		return, 1
        JUMP: IF 'Cancel' EQ dialog_message(msg,/information,/cancel) THEN return, 0

  	ENDWHILE
END



pro PMI__Button__Event__MOCO_3D_DCE_2CM, ev

	PMI__Info, ev.top, Status=Status, Stdy=Stdy
	PMI__Message, status, 'Preparing calculation..'

    IF NOT PMI__Button__Input__MOCO_3D_DCE_2CM(ev.top,series,aif,in,win) THEN RETURN

	PMI__Message, status, 'Preparing calculation..'

	time = Series->t() - Series->t(0)
	Source = Series->Read(Stdy->DataPath())

    PMI__Message, status, 'Calculating..'
tt=systime(1)
	Source = TRANSPOSE(Source, [3,0,1,2])
    MOCO_3D_DCE_2CM = OBJ_NEW('MOCO_3D_DCE_2CM', ptr_new(Source), in.res, in.prec, [Time, aif, in.nb], Win=win)
    Source = TRANSPOSE(MOCO_3D_DCE_2CM->deformed(), [1,2,3,0])
    OBJ_DESTROY, MOCO_3D_DCE_2CM
    Dom = {z:Series->z(), t:Series->t(), m:Series->m()}
    Corr = Stdy->New('SERIES', Domain=Dom,  Name=Series->name() + '[Motion-free]' )
	Corr -> Write, Stdy->DataPath(), Source
	Corr -> Trim, Series->Trim()
print,(systime(1)-tt)/60.

    PMI__Control, ev.top, /refresh
end


pro PMI__Button__Control__MOCO_3D_DCE_2CM, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 0)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__MOCO_3D_DCE_2CM, parent,value=value,separator=separator

    if n_elements(value) eq 0 then value = 'PK motion correction'

    id = widget_button(parent $
    ,   value = value  $
    ,  	event_pro = 'PMI__Button__Event__MOCO_3D_DCE_2CM' $
    ,	pro_set_value = 'PMI__Button__Control__MOCO_3D_DCE_2CM' $
    ,  	separator = separator )

    return, id
end

