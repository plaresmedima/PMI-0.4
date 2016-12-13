;
;
;    Copyright (C) 2005 Steven Sourbron
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


function convolution_matrix_prefilter, c, o

	n = n_elements(c)
	v = fltarr(n)

	if o eq 2 then begin
		v[0] = (c[0]+4.0*c[0]+c[1])/6.0
		for i=1L,n-2 do v[i] = (c[i-1]+4.0*c[i]+c[i+1])/6.0
		v[n-1] = (c[n-2]+4.0*c[n-1]+c[n-1])/6.0
	endif

	return, v

end

function convolution_matrix, vec, QUAD=quad

	c = vec
	n = n_elements(c)

	if n_elements(quad) eq 0 then quad = 'O2'


	;Volterra convolution


	if quad eq 'O1' then begin
		mat = fltarr(n,n)
		for i=0L,n-1 do begin
	  		for j=0L,i do begin
            	mat[j,i] = c[i-j]
    		endfor
    	endfor
    endif
    if quad eq 'O2' then begin
    	mat = fltarr(n,n)
    	for i=1L,n-1 do begin
    		mat[i,i] = 2*c[0] + c[1]
    		for j=1,i-1 do begin
    			mat[i-j,i] = c[j-1]+4*c[j]+c[j+1]
    		endfor
    		mat[0,i] = c[i-1] + 2*c[i]
    	endfor
    	mat = mat/6D
    endif

	if quad eq 'Hybridf' then begin
		mat = fltarr(n,n)
		for i=1L,n-1 do begin
			for j=0L,i-1 do begin
				mat[j,i]= (c[i-j]+c[i-j-1])/2D
			endfor
		endfor
	endif

	if quad eq 'Hybridb' then begin
		mat = fltarr(n,n)
		for i=1L,n-1 do begin
			for j=1L,i do begin
				mat[j,i]= (c[i-j]+c[i-j+1])/2D
			endfor
		endfor
	endif


	;Singular convolution


	if quad eq 'O2+' then begin
		c = convolution_matrix_prefilter(c, 2)
		mat = convolution_matrix(c,quad='O1')
	endif

	if quad eq 'Hybridf+' then begin
		mat = fltarr(n,n)
		for i=0L,n-1 do begin
			for j=0L,i-1 do begin
				mat[j,i]= (c[i-j]+c[i-j-1])/2
			endfor
			mat[i,i] = (c[0]+0)/2
		endfor
	endif

	if quad eq 'Hybridb+' then begin
		mat = fltarr(n,n)
		for i=0L,n-1 do begin
			if i lt n-1 then begin
				mat[0,i] = (c[i]+c[i+1])/2
				mat[i+1,i] = (c[0]+0)/2
			endif else begin
				mat[0,i] = (c[i]+c[i])/2
			endelse
			for j=1L,i do begin
				mat[j,i] = (c[i-j]+c[i-j+1])/2
			endfor
		endfor
	endif


	;Circular convolution


	if quad eq 'CIRC' then begin
		mat = fltarr(n,n)
		for i=0L,n-1 do begin
			mat[i,*] = c
			c = shift(c, 1)
		endfor
	endif

	if quad eq 'CIRC+' then begin
		v = fltarr(2L*n-1)
		v[0:n-1] = convolution_matrix_prefilter(c, 2)
		mat = convolution_matrix(v,QUAD='CIRC')
	endif

	return, mat
end
