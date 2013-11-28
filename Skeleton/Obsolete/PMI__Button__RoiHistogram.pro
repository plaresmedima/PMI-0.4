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


pro PMI__Button__Output__RoiHistogram, $
	RegionName,	SeriesName,	path, $
	perc, npix, avr, sdev, minv, maxv, mode, $
	xhist, yhist

	window, xsize=1200, ysize=600

 	plot, position=[0.1,0.2,0.5,0.9]  $
 	,	Title = 'ROI Histogram' $
	, 	xhist, yhist, psym=10 $
	, 	/xstyle, /ystyle $
	, 	background=255, color=0 $
	, 	xtitle = 'ROI Values', ytitle='Frequency (%)' $
	, 	charsize=2.0, charthick=2.0, xthick=2.0, ythick=2.0, thick=2.0

	top=0.875 & dy=0.05 & x0=0.525 & charsize=2.0 & charthick=2.0

	xyouts,x0,top-0*dy,'SERIES: ' + SeriesName 	,color=0,/normal,charsize=charsize,charthick=charthick
	xyouts,x0,top-1*dy,'REGION: ' + RegionName 	,color=0,/normal,charsize=charsize,charthick=charthick

	xyouts,x0,top-3*dy	,'Nr. of values  ' + npix	,color=0,/normal,charsize=charsize,charthick=charthick
	xyouts,x0,top-4*dy	,'Mean           ' + avr 	,color=0,/normal,charsize=charsize,charthick=charthick
	xyouts,x0,top-5*dy	,'Stdev          ' + sdev	,color=0,/normal,charsize=charsize,charthick=charthick
	xyouts,x0,top-6*dy	,'Minimum        ' + minv	,color=0,/normal,charsize=charsize,charthick=charthick
	xyouts,x0,top-7*dy	,'Maximum        ' + maxv	,color=0,/normal,charsize=charsize,charthick=charthick
	xyouts,x0,top-8*dy	,'1% percentile  ' + perc[0],color=0,/normal,charsize=charsize,charthick=charthick
	xyouts,x0,top-9*dy	,'10% percentile ' + perc[1],color=0,/normal,charsize=charsize,charthick=charthick
	xyouts,x0,top-10*dy	,'25% percentile ' + perc[2],color=0,/normal,charsize=charsize,charthick=charthick
	xyouts,x0,top-11*dy	,'Median         ' + perc[3],color=0,/normal,charsize=charsize,charthick=charthick
	xyouts,x0,top-12*dy	,'75% percentile ' + perc[4],color=0,/normal,charsize=charsize,charthick=charthick
	xyouts,x0,top-13*dy	,'90% percentile ' + perc[5],color=0,/normal,charsize=charsize,charthick=charthick
	xyouts,x0,top-14*dy	,'99% percentile ' + perc[6],color=0,/normal,charsize=charsize,charthick=charthick
	xyouts,x0,top-15*dy	,'Mode ' 		   + mode	,color=0,/normal,charsize=charsize,charthick=charthick

	Path = Path + 'ROI statistics'
	file_mkdir, Path
	File = Path + '\' + SeriesName + '_' + RegionName + '_Histogram'
	write_tiff, File + '.tif', reverse(tvrd(/true),3)
	PMI__WritePlot, File +'.txt', xhist, yhist
end

pro PMI__Button__Event__RoiHistogram, ev

	PMI__info, ev.top, Series=Data, Region=Reg, Stdy=Stdy, status=status

	in = PMI__Form(ev.top, Title='Histogram options', [$
		ptr_new({Type:'VALUE',Tag:'nbins', Label:'Number of bins', Value:100L}), $
		ptr_new({Type:'VALUE',Tag:'pmax', Label:'Maximum value (%)', Value:100E}), $
		ptr_new({Type:'VALUE',Tag:'pmin', Label:'Minimum value (%)', Value:0E})])
		IF in.cancel THEN return

	if     (in.pmin ge in.pmax) $
	    or (in.pmin gt 100) $
	    or (in.pmax gt 100) $
	    or (in.pmin lt 0) $
	    or (in.pmax lt 0) then return

	v = PMI__RoiValues(Stdy->DataPath(), Data,Reg,status,cnt=npix)
	if npix lt 2 then return

	minv = min(v)
	maxv = max(v)
	perc = Percentiles(v,[1,10,25,50,75,90,99])

	if in.pmin ne 0 then begin
		p = Percentiles(v,[in.pmin])
		minb = p[0]
	endif else minb=minv
	if in.pmax ne 100 then begin
		p = Percentiles(v,[in.pmax])
		maxb = p[0]
	endif else maxb=maxv

	binsize = (maxb-minb)/in.nbins
	yhist 	= histogram(v,min=minb,max=maxb,binsize=binsize)
	xhist   = binsize*findgen(in.nbins) + minb + binsize/2
	ymax 	= max(yhist,imax)

	PMI__Button__Output__RoiHistogram $
	,	cleanstr(Reg->Name()) $
	,	cleanstr(Data->Name()) $
	,	Stdy->DataPath() $
	,	strcompress(perc,/remove_all) $
	,	strcompress(npix,/remove_all) $
	,	strcompress(mean(v),/remove_all) $
	,	strcompress(stddev(v),/remove_all) $
	,	strcompress(minv,/remove_all) $
	,	strcompress(maxv,/remove_all) $
	,	strcompress(xhist[imax],/remove_all) $
	,	xhist $
	,	100.0*yhist/total(yhist)

end

pro PMI__Button__Control__RoiHistogram, id, v

	PMI__Info, tlb(id), Stdy=Stdy, Series=Series, Region=Region
	sensitive = obj_valid(Series) and obj_valid(Region)
    widget_control, id, sensitive=sensitive
end
function PMI__Button__RoiHistogram, parent, separator=separator

 	return, widget_button(parent, $
 		separator = separator, $
 	 	value = 'ROI statistics',	$
	 	event_pro = 'PMI__Button__Event__RoiHistogram',	$
		pro_set_value = 'PMI__Button__Control__RoiHistogram' )

end
