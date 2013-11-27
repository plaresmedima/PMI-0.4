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



function PMI__Button__DisplayImage__ImageWithUnderlay__Colorbar, Path, Overlay, Underlay, i, j, Region, scale=scale, cnt=np

    if n_elements(Region) ne 0 $
    then p = Region->where(Path,i,j,n=np) $
    else p = where(Overlay->Read(Path,i,j) ne 0, np)

    imU = Underlay -> Image(Path,i,j,clrstrip=keyword_set(strip),/bits24)
    imO = Overlay  -> Image(Path,i,j,clrstrip=keyword_set(strip),/bits24)

    d = Underlay -> d()

    for k=0L,np-1 do begin
       xy = reform_ind(d[0:1],ind=p[k])
       imU[*,xy[0],xy[1]] = ImO[*,xy[0],xy[1]]
    endfor

	d = [(size(imU))(2),(size(imU))(3)]

	if d[0] lt 128 then d[0:1] = floor(4.*d[0:1])
	if d[0] lt 256 then d[0:1] = floor(2.*d[0:1])

	imU = rebin(imU,3,scale*d[0],scale*d[1],/sample)

    return, ImU
end

pro PMI__Button__DisplayImage__ExportWithUnderlay__Colorbar $
,   Path $
,   Underlay $
,   Overlay $
,   Region $
,   Display $
,   top $
,   unit $
,   im_name $
,   suffix = suffix $
,   w_slice = w_slice $
,	scale = scale $
,   bar = bar $
,   steps

    d = underlay -> d()
    v = overlay->trim()
    v0 = overlay->trim(0)
    v1 = overlay->trim(1)

	Display->GET, CursorPos=current

    R = overlay->Clr(/R)
    G = overlay->Clr(/G)
    B = overlay->Clr(/B)



    for i=0L,d[2]-1 do begin
       for j=0L,d[3]-1 do begin

		if keyword_set(w_slice) then begin
			i = current(2)
			j = current(3)
		endif


        ImU = PMI__Button__DisplayImage__ImageWithUnderlay__Colorbar(Path, Overlay, Underlay, i,j, Region, scale=scale, cnt=np)

          ij = '(' + strnr(i,3) + ',' + strnr(j,3) + ')'
          file_ij = Path + cleanstr(im_name) + ij

		  if keyword_set(w_slice) then file_ij = Path + im_name

		 xsize = (size(imu))(2) + 100.
		 im_xsize = (size(imu))(2)/xsize
		 ysize =(size(imu))(3)

		 im_pos = [0.0,0.0, im_xsize,1.0]

		 window, 2, xsize=xsize, ysize= ysize

         TVImage, imu, Position= im_pos

		 TVLCT, R,G,B
         if keyword_set(bar) then colorbar,  vertical=1, division = steps, position = [.87, 0.15, 0.95, 0.90], NCOLORS=256, RANGE=[v0,v1], title = top, unit = unit, FORMAT = '(G11.4)'

		 IF (!D.Flags AND 256) NE 0 THEN BEGIN
			   Device, Get_Decomposed=theDecomposedState, Get_Visual_Depth=theDepth
			   IF theDepth GT 8 THEN BEGIN
			      Device, Decomposed=1
			      color24 = 1
			   ENDIF ELSE truecolor = 0
  		 ENDIF ELSE BEGIN
			   color24 = 0
			   theDepth = 8
		 ENDELSE

		 image = TVRD(True=color24)

		 if suffix eq 'tiff' then begin
		 	IF color24 THEN BEGIN
            image3D = Reverse(image,3)
         	ENDIF ELSE BEGIN
            TVLCT, r, g, b, /Get
            image3D = [ [[r[image]]], [[g[image]]], [[b[image]]] ]
            image3D = Transpose(image3d,[2,0,1])
            image3D = Reverse(Temporary(image3D), 3)
         	ENDELSE
         	Write_TIFF, file_ij+ '.tif', image3D, 1
         endif

		 if suffix eq 'png' then begin
			 IF color24 THEN BEGIN
	            Write_PNG, file_ij+ '.png', image, _Extra=extra
	         ENDIF ELSE BEGIN
	            TVLCT, r, g, b, /Get
	            image2D = image
	            Write_PNG, file_ij+'.png', Reverse(image2D,2), r, g, b, _Extra=extra
	         ENDELSE
	     endif

		 if suffix eq 'ps' then begin
			a = PSWINDOW()
   			SET_PLOT, 'PS'
   			DEVICE,filename= file_ij+'.ps',/Color,BITS_PER_PIXEL=8, _EXTRA=a

			POLYFILL, [1,1,0,0,1], [1,0,0,1,1], /NORMAL, COLOR=black

			TVImage, imu, Position= im_pos

			TVLCT, R,G,B
            if keyword_set(bar) then colorbar,vertical=1, division = steps, position = [.87, 0.1, 0.95, 0.90], NCOLORS=256, RANGE=[v0,v1], title = top, unit = unit, FORMAT = '(G11.4)'

          	Device, /Close_File

          	loadct, 0

         	Set_plot, 'win'

		  endif

		 loadct, 0

    	 DEVICE, decomposed=0

		 if keyword_set(w_slice) then return
       endfor
    endfor



end

pro PMI__Button__Event__DisplayImage, ev

    PMI__info, ev.top, State=State, Stdy=Stdy, Series=Overlay, Display = Display

    Regions = Stdy->names(1,n1,Dim=Overlay->d(),Ind=i1)
    Expose = ['All non-zero values']
	suffix = ['tiff','png','ps']
	w_slice = ['Current slice', 'All slices']

    ; Determine title and unit of series
    name = Overlay->name()
    tb = strpos(name,'[')
    te = strpos(name,':')
    tl = te-tb-1
    ue = strpos(name,']')
    ul = ue -te-1

	Display->GET, CursorPos=Pos

	im_name = cleanstr(Overlay->Name()) + '(' + strnr(Pos[2],3) + ',' + strnr(Pos[3],3) + ')'

    if ((tb eq -1) or (te eq -1)) then begin title = ['title']
    endif else begin
        title = strmid(name,tb+1,tl)
    endelse

    if ((te eq -1) or (ue eq -1)) then begin unit = ['unit']
    endif else begin
        unit = strmid(name,te+1,ul)
    endelse

    If n1 gt 0 then Expose = [Expose,Regions]

	in = PMI__Form(ev.top, Title='Overlay export setup', [$
	ptr_new({Type:'DROPLIST',Tag:'underlay', Label:'Underlay', Value:Stdy->names(0,n0,Dim=Overlay->d(),Ind=i0)}), $
	ptr_new({Type:'DROPLIST',Tag:'mask'  ,Label:'Expose..', Value:Expose}), $
	ptr_new({Type:'DROPLIST',Tag:'format',Label:'Export format?', Value:Suffix}), $
	ptr_new({Type:'DROPLIST',Tag:'bar'   ,Label:'Color bar?', Value:['Yes','No']}),$
	ptr_new({Type:'DROPLIST',Tag:'slices',Label:'Which slices?', Value:w_slice}), $
	ptr_new({Type:'VALUE'	,Tag:'scale' ,Label:'Scale factor', Value:1L}),$
	ptr_new({Type:'VALUE'	,Tag:'title' ,Label:'Title?', Value:title}),$
	ptr_new({Type:'VALUE'	,Tag:'unit'  ,Label:'Unit?', Value:unit}),$
	ptr_new({Type:'VALUE'	,Tag:'nsteps',Label:'Number of steps?', Value:5L}),$
	ptr_new({Type:'VALUE'	,Tag:'name'  ,Label:'Image Name?', Value:im_name})])
	if in.cancel then return

    if in.mask gt 0 then Region = Stdy -> Obj(1,i1[in.mask-1])

    PMI__Button__DisplayImage__ExportWithUnderlay__Colorbar $
    ,  Stdy->DataPath() $
    ,  Stdy -> Obj(0,i0[in.underlay]) $
    ,  Overlay $
    ,  Region $
    ,  Display $
    ,  in.title $
    ,  in.unit $
    ,  in.name $
    ,  suffix = suffix[in.format]$
	,  w_slice = in.slices eq 0$
	,  scale  = in.scale $
    ,  bar = in.bar eq 0 $
    ,  in.nsteps

end

pro PMI__Button__Control__DisplayImage, id, v

	PMI__Info, tlb(id), Stdy=Stdy, Series=Series, Viewer=Viewer
;	sensitive = (Viewer ne 'PMI__DISPLAY__2DVIEW') AND obj_valid(Stdy)
	sensitive = obj_valid(Series)
	widget_control, id, sensitive = sensitive
end

function PMI__Button__DisplayImage, parent, separator=separator, value=value

	if n_elements(value) eq 0 then value    = 'Color Image'

    return, widget_button(parent,   $
      	value  = value,   $
     	event_pro = 'PMI__Button__Event__DisplayImage',  $
     	pro_set_value = 'PMI__Button__Control__DisplayImage',$
      	separator = separator  )
end