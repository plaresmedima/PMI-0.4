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


function PMI__Button__Input__VolumeOfDistribution $
   ,    ev $
   ,    Stdy = Stdy $
   ,    status = status $
   ,    time = time $
   ,    vof_int = vof_int $
   ,    Series = series $
   ,	nb = nb $
   ,	relative = relative

  	PMI__Info, ev.top, Status=Status, Stdy=Stdy

    Series = Stdy->names(0,DefDim=3,ind=ind,sel=sel)

	in = PMI__Form(ev.top, Title='Perfusion analysis setup', [$
	ptr_new({Type:'DROPLIST',Tag:'ser', Label:'Dynamic series', Value:Series, Select:sel}), $
	ptr_new({Type:'DROPLIST',Tag:'vof', Label:'Venous Region', Value:Stdy->names(1), Select:Stdy->sel(1)}), $
	ptr_new({Type:'DROPLIST',Tag:'rel', Label:'Approximate tracer concentrations by:', Value:['Signal Enhancement','Relative Signal Enhancement'], Select:0}), $
	ptr_new({Type:'VALUE'	,Tag:'nb' , Label:'Length of baseline (# of dynamics)', Value:10B}),$
	ptr_new({Type:'VALUE'	,Tag:'hct', Label:'Patients hematocrit', Value:0.45})])
	IF in.cancel THEN return, 0

    Series = Stdy->Obj(0,ind[in.ser])
    Vof = Stdy->Obj(1,in.vof)
    nb = in.nb
	relative = in.rel
    Time = Series->c(1)
    Time = Time-Time[0]

    Vof = PMI__RoiCurve(Stdy->DataPath(), Series, Vof, Status, cnt=cnt)
    if cnt eq 0 then return, 0
    Vof = LMU__Enhancement(Vof,in.nb,relative=in.rel)/(1-in.hct)
    Vof = Intvector(time,Vof)
    Vof_Int = float(Vof[n_elements(Vof)-1])

  return, 1
end


pro PMI__Button__Event__VolumeOfDistribution, ev


    if not PMI__Button__Input__VolumeOfDistribution( $
       ev $
    ,  Stdy = Stdy $
    ,  status = status $
    ,  time = time $
    ,  vof_int = vof_int $
    ,  	Series = series $
    ,	nb = nb $
   	,	relative = relative $
    )  then return



	VD = Stdy->New('SERIES', Default=Series, ClrWin=[0E,100], Name='Compartment Volume (ml/100ml)')
	VD -> t, Series->t(0)

	d = Series->d()

	for i=0L,d[2]-1 do begin

 		Slice = strcompress(i+1,/remove_all)+'/'+strcompress(d[2],/remove_all)
		PMI__Message, Status, 'Loading ' + slice
		im = Series->Read(Stdy->DataPath(), i, -1)
		im = reform(im,d[0]*d[1],d[3],/overwrite)
		PMI__Message, Status, 'Calculating ' + slice
		if nb eq 1 then im0 = reform(im[*,0]) else im0 = total(im[*,0:nb-1],2)/nb
		im0 = rebin(im0,d[0]*d[1],d[3])
    	im = im - im0
    	if relative then im = im/im0
		im[*,0] = 100*IntArray(time,im)/vof_int
		PMI__Message, Status, 'Saving ' + slice
		VD -> Write, Stdy->DataPath(), im[*,0], i
	endfor


    PMI__Control, ev.top, /refresh
end

pro PMI__Button__Control__VolumeOfDistribution, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		Regions = Stdy->Names(1,nr)
		sensitive = (ns gt 0) and (nr gt 0)
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end


function PMI__Button__VolumeOfDistribution, parent,value=value,separator=separator

    if n_elements(value) eq 0 then value = 'Volume of distribution with venous outflow curve (Pixel)'

    id = widget_button(parent                $
    ,   value      = value  $
    ,  	event_pro   = 'PMI__Button__Event__VolumeOfDistribution'  $
    ,	pro_set_value 	= 'PMI__Button__Control__VolumeOfDistribution' $
    ,  	separator   = separator                 )

    return, id
end

