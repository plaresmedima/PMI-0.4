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

function TumorPixelAnalysis__input $
   ,	top $
   ,	Stdy = Stdy $
   ,	status = status $
   ,	time = time $
   ,	pcurve = pcurve $
   ,	aif = aif $
   ,	units = units $
   ,	Roi = roi $
   ,	Series = series $
   ,	ev =ev

   	PMI__Info, top, Status=Status, Stdy=Stdy

   	Series = Stdy->Names(0,DefDim=3,ind=ind,sel=sel)
    Units = ['Linear (a.u.)','Linear (%)']

	in = PMI__Form(top, Title='Perfusion analysis setup', [$
		ptr_new({Type:'DROPLIST',Tag:'ser', Label:'Dynamic series', Value:Series, Select:sel}), $
		ptr_new({Type:'DROPLIST',Tag:'aif', Label:'Arterial Region', Value:Stdy->names(1), Select:Stdy->sel(1)}), $
		ptr_new({Type:'DROPLIST',Tag:'roi', Label:'Tissue Region', Value:Stdy->names(1), Select:Stdy->sel(1)}), $
		ptr_new({Type:'DROPLIST',Tag:'rel', Label:'Signal model:', Value:Units, Select:0}), $
		ptr_new({Type:'VALUE'	,Tag:'nb' , Label:'Length of baseline (# of dynamics)', Value:5B}),$
		ptr_new({Type:'VALUE'	,Tag:'hct', Label:'Patients hematocrit', Value:0.45})])
		IF in.cancel THEN return, 0

    Series = Stdy->Obj(0,ind[in.ser])
    Art = Stdy->Obj(1,in.aif)
    Roi = Stdy->Obj(1,in.roi)
    Units = Units[in.rel]


    Time = Series->c(1)
    Time = Time-Time[0]


    Aif = PMI__RoiCurve(Stdy->DataPath(), Series, Art, id, cnt=cnt)
    if cnt eq 0 then begin
    	ok = dialog_message(/information,'Arterial ROI is empty!')
    	return, 0
    endif
    Aif = LMU__Enhancement(Aif,in.nb, relative=in.rel eq 1)/(1-in.hct)

  	pcurve = PMI__PixelCurve(Stdy->DataPath(), Series, Roi, status, cnt=cnt)
  	if cnt eq 0 then return, 0

  	if in.nb eq 1 then baseline = reform(pcurve[*,0]) $
  	else baseline = total(pcurve[*,0:in.nb-1],2)/in.nb

  	dim= size(pcurve,/dimension)
  	baseline = rebin(baseline,dim[0],dim[1])
  	pcurve = pcurve-baseline
  	IF in.rel eq 1 THEN pcurve = pcurve/baseline

  	return, 1
end
