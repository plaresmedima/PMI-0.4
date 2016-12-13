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

function TumorRoiAnalysis__input $
, 	top $
,	Status	 = id $
,	time	 = time $
,	curve 	 = roicurve $
,	aif 	 = aif $
,	units 	 = units $
,	ROI 	 = roi $
,	nb=nb $
,	ev = ev

	PMI__Info, top, Status=id, Stdy=Stdy
	PMI__Message, id, 'Getting input..'

   	Series = Stdy->Names(0,DefDim=3,ind=ind,sel=sel)
    Units = ['Linear (a.u.)','Linear (%)','DSC-MRI']

	in = PMI__Form(ev.top, Title='Perfusion analysis setup', [$
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
    nb = in.nb

    Time = Series->c(1)
    Time = Time-Time[0]

    Aif = PMI__RoiCurve(Stdy->DataPath(), Series, Art, id, cnt=cnt)
    if cnt eq 0 then begin
    	ok = dialog_message(/information,'Region <' + Art->Name() + '> is not defined on Series <' + Series->Name() + '>')
    	return, 0
    endif
	RoiCurve = PMI__RoiCurve(Stdy->DataPath(), Series, Roi, id, cnt=cnt)
    if cnt eq 0 then begin
    	ok = dialog_message(/information,'Region <' + Roi->Name() + '> is not defined on Series <' + Series->Name() + '>')
    	return, 0
    endif

	Aif = LMU__Enhancement(Aif,nb,relative=in.rel)/(1-in.hct)
	RoiCurve = LMU__Enhancement(RoiCurve,nb,relative=in.rel)

	return, 1

end

