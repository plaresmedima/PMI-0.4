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


function PMI__Function__ImageWithUnderlay, Path, Overlay, Underlay, i, j, Region, strip=strip, cnt=np

	if n_elements(Region) ne 0 $
	then p = Region->where(Path,i,j,n=np) $
	else p = where(Overlay->Image(Path,i,j) ne 0, np)

	if np eq 0 then return, 0B

	imU = Underlay 	-> Image(Path,i,j,clrstrip=keyword_set(strip),/bits24)
	imO = Overlay 	-> Image(Path,i,j,clrstrip=keyword_set(strip),/bits24)

	d = Underlay -> d()

	for k=0L,np-1 do begin
		xy = reform_ind(d[0:1],ind=p[k])
		imU[*,xy[0],xy[1]] = ImO[*,xy[0],xy[1]]
	endfor

	if keyword_set(strip) then imU[*,d[0]:*,*] = ImO[*,d[0]:*,*]

	return, ImU
end

function PMI__Function__VolumeWithUnderlay, Path, Overlay, Underlay, j, Region, nx, strip=strip

	d  = Underlay -> d()
	ny = ceil(d[2]/float(nx))
	dx = nx*d[0]
	dy = ny*d[1]

	if keyword_set(strip) then begin
		ds = 20
		im = bytarr(3,dx+ds, dy)
		R = Overlay->Clr(/R)
		G = Overlay->Clr(/G)
		B = Overlay->Clr(/B)
		for i=0E,dy-1 do begin
			clr = floor(255*i/(dy-1))
			im[0,dx:*,i] = R[clr]
			im[1,dx:*,i] = G[clr]
			im[2,dx:*,i] = B[clr]
		endfor
	endif else im = bytarr(3,dx,dy)

	for i=0L,d[2]-1 do begin
		r = reform_ind([nx,ny],ind=i)
		r[1] = ny-1-r[1]
		im_i = PMI__Function__ImageWithUnderlay(Path, Overlay, Underlay,i,j, Region, cnt=np)
		im[*, r[0]*d[0]:(r[0]+1)*d[0]-1, r[1]*d[1]:(r[1]+1)*d[1]-1] = im_i
	endfor

	return, Im
end

pro PMI__Button__Event__DisplayVolume, ev

	PMI__info, ev.top, State=State, Stdy=Stdy, Series=Overlay

	suffix = ['tiff','bmp','jpeg','iImage']
	Regions = Stdy->names(1,n1,Dim=Overlay->d(),Ind=i1)
	Expose = ['All non-zero values']
	If n1 gt 0 then Expose = [Expose,Regions]

	in = PMI__Form(ev.top, Title='Overlay export setup', [$
	ptr_new({Type:'DROPLIST',Tag:'underlay', Label:'Underlay', Value:Stdy->names(0,n0,Dim=Overlay->d(),Ind=i0)}), $
	ptr_new({Type:'DROPLIST',Tag:'mask', Label:'Expose..', Value:Expose}), $
	ptr_new({Type:'DROPLIST',Tag:'format', Label:'Export format?', Value:Suffix}), $
	ptr_new({Type:'DROPLIST',Tag:'strip' , Label:'Color strip?', Value:['Yes','No']}),$
	ptr_new({Type:'VALUE'	,Tag:'ncols', Label:'Number of columns?', Value:4L})])
	if in.cancel then return

	if in.mask gt 0 then Region = Stdy -> Obj(1,i1[in.mask-1])

	Path = Stdy->ExportPath()

	Underlay = Stdy -> Obj(0,i0[in.underlay])
	d = Underlay -> d()

	PMI__Control, ev.top, Viewer = 'PMI__Display__Image', Display=Display

	for j=0L,d[3]-1 do begin

		ImU = PMI__Function__VolumeWithUnderlay(Path, Overlay, Underlay, j, Region, in.ncols, strip=in.strip eq 0)
		file = Path + cleanstr(Overlay->Name()) + '(' + strnr(j,3) + ').' + suffix[in.format]

		Display -> Set, Image=ImU, /Refresh

		case suffix[in.format]  of
			'tiff'	:write_tiff	, file, reverse(imU,3)
			'bmp'	:write_bmp	, file, reverse(imU,1)
			'jpeg'	:write_jpeg	, file, imU, /true, quality=100
			'iImage': iimage, imU
    	endcase
	endfor
end

pro PMI__Button__Control__DisplayVolume, id, v

	PMI__Info, tlb(id), Series=Series
	sensitive = obj_valid(Series)
	widget_control, id, sensitive = sensitive
end


function PMI__Button__DisplayVolume, parent, value=value, separator=separator

	PMI__Display__Image__Define

	if n_elements(value) eq 0 then value  = 'Color Volume'

	return, widget_button(parent,$
		value = value,	$
		event_pro = 'PMI__Button__Event__DisplayVolume',	$
		pro_set_value = 'PMI__Button__Control__DisplayVolume',$
		separator = separator	)

end