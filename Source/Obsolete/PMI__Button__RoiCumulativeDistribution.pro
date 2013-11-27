;
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


pro PMI__Button__Event__RoiCumulativeDistribution, ev

	PMI__info, ev.top, Series=Data, Region=Reg, Stdy=Stdy, status=status

	v = PMI__RoiValues(Stdy->DataPath(), Data,Reg,status,cnt=n)
	if n lt 2 then return

	v = v[sort(v)]
;	y = 100*findgen(n)/(n-1E)
	y = uniq(v)
	v = v[y]
	y = 100E*(y+1E)/n

	window, xsize=1200, ysize=600

 	plot, position=[0.1,0.2,0.5,0.9]  $
 	,	Title = 'ROI Cumulative Distribution' $
	, 	v, y $
	, 	/xstyle, /ystyle $
	, 	background=255, color=0 $
	, 	xtitle = 'Pixel values', ytitle='Percentile (%)' $
	, 	charsize=2.0, charthick=2.0, xthick=2.0, ythick=2.0, thick=2.0

	top=0.875 & dy=0.05 & x0=0.525 & charsize=2.0 & charthick=2.0

	xyouts,x0,top-0*dy,'SERIES: ' + Data->Name() 	,color=0,/normal,charsize=charsize,charthick=charthick
	xyouts,x0,top-1*dy,'REGION: ' + Reg->Name() 	,color=0,/normal,charsize=charsize,charthick=charthick

	Path = Stdy->Datapath() + 'ROI statistics'
	file_mkdir, Path
	File = Path + '\' + Data->Name() + '_' + Reg->Name() + '_CumulativeDistribution'
	write_tiff,  file + '.tif', reverse(tvrd(/true),3)
	PMI__WritePlot, file +'.txt', v, y

end

pro PMI__Button__Control__RoiCumulativeDistribution, id, v

	PMI__Info, tlb(id), Stdy=Stdy, Series=Series, Region=Region
	sensitive = obj_valid(Series) and obj_valid(Region)
    widget_control, id, sensitive=sensitive
end
function PMI__Button__RoiCumulativeDistribution, parent, separator=separator

 	return, widget_button(parent, $
 		separator = separator, $
 	 	value = 'ROI cumulative distribution',	$
	 	event_pro = 'PMI__Button__Event__RoiCumulativeDistribution',	$
		pro_set_value = 'PMI__Button__Control__RoiCumulativeDistribution' )

end
