pro PMI__Button__Event__iBEAt_DCE_AIF_Segmentation, ev, in

	PMI__Info, ev.top, Status=Status, Stdy=Stdy, Region=Region
	PMI__Message, Status, 'Creating AIF ROI'

    Series = Stdy->Names(0,ns,DefDim=3,ind=ind,sel=sel)

	v = PMI__Form(ev.top, Title='iBEAt DCE-MRI aorta segmentation (axial slice only)', $
	[ ptr_new({Type:'DROPLIST',Tag:'series',Label: 'Dynamic series', Value:Series, Select:sel}) ])

	if v.cancel then return

	PMI__Message, Status, 'Segmenting AIF'

	Series = Stdy->Obj(0,ind[v.series])

	time = Series->t()
	time = float(time-time[0])
	d = Series->d()

	MaxIm = fltarr(d[0]*d[1])
	normalisedIm = fltarr(d[0]*d[1])

	P0 = Series -> Read(Stdy->DataPath(),8,0) ; 8th - axial slice for AIF selection
    nozero = where(P0 NE 0, cnt_nozero)

	   if cnt_nozero GT 0 then begin
	      for j=0L,d[3]-1 do begin

			PMI__Message, Status, 'Calculating', (8*d[3]+j)/(d[2]*d[3]-1E)
			P = Series -> Read(Stdy->DataPath(),8,j)

            P = P[nozero]-P0[nozero] ;max image: current baseline image for threshold

			    if j eq 0 then begin
				    MaxIm[nozero] = P

			    endif else begin
				    ind = where(P gt MaxIm[nozero], cnt)
				    if cnt gt 0 then begin
					    MaxIm[nozero[ind]] = P[ind]
				    endif
                endelse
	  	    endfor

          normalisedIm =((MaxIm-min(MaxIm))/(max(MaxIm)-min(MaxIm)))*255 ; normalise

       endif


   ; segmentation of Aorta
    AIF_Im =  fltarr(d[0],d[1])
    AIF_Im = REFORM(normalisedIm,d[0],d[1])

    ; smooth image and equalise the histogram to increase contrast of aorta for cases with lower contrast
    smoothed_image = SMOOTH(AIF_Im, 5, /EDGE_TRUNCATE) ; smooth image to be thresholded
    AIF_Im_New  = HIST_EQUAL(smoothed_image, MINV = 20, MAXV = 220, TOP = 255) ; equalise contrast to allow AIF region threshold >100 on all cases

    ; erode to remove background elements
    radius = 5
    strucElem = SHIFT(DIST(2*radius+1), radius, radius) LE radius
    morphImg = ERODE(AIF_Im_New, strucElem, /GRAY)

    ; threshold the aorta
    threshImg = morphImg GE 130 ; BINARY IMAGE; lower threshold corrupts object shape


    ; increase size by dilation
    radius_NEW = 4;4
    strucElem_NEW = SHIFT(DIST(2*radius_NEW+1), radius_NEW, radius_NEW) LE radius_NEW
    morphThresh = DILATE(threshImg, strucElem_NEW)  ; INCREASE SIZE OF AORTA

    ; remove background elements and select aorta
    radhit = 6
    radmiss = 20
    hit = SHIFT(DIST(2*radhit+1), radhit, radhit) LE radhit
    miss = SHIFT(DIST(2*radmiss+1), radmiss, radmiss) GE radmiss

    matches = MORPH_HITORMISS(morphThresh, hit, miss) ; remove any remaining background regions

    dmatches = DILATE(matches, hit)
    ; if no ROI found
    if max(dmatches) EQ 0 then begin
       messageDialog = dialog_message(/information,'AIF ROI not found')
       return
    end

    labels = LABEL_REGION(dmatches) ; label foreground region(s)

    h = HISTOGRAM(labels, MIN=1, REVERSE_INDICES=r)

    if N_ELEMENTS(h) GE 1 then begin  ; if regions >1 then remove everyting except AIF region
        for i = 1, N_ELEMENTS(h)-1 do begin
            IF r[i] NE r[i+1] THEN dmatches[r[r[i] : r[i+1]-1]] = 0
        endfor

    endif

   ; create Region on display
   Region_AIF    = Stdy -> New('REGION' $
	 , name 	= STRCOMPRESS('AIF_ROI',/REMOVE_ALL)  $
	 ,	Domain 	= Series->dom() $
     ,	Color 	= Series->Clr(SAT='G'))

   Region_AIF -> Write, Stdy->DataPath(), dmatches, 8 ; axial slice with AIF

   ; extrude region to other dynamics
   Region = Region_AIF
   Path = Stdy->DataPath()

    	Extrude = Stdy->New('REGION' $
	,	Name 	= Region->name()+'[Extrude]' $
	,	Default	= Region $
	,	Domain  = Series->Dom() )

    r=[0L,d[3]-1]
    nr = r[1]-r[0]+1
    dR = Region	-> d()

    for i=0L,d[2]-1 do begin

		PMI__Message, status, 'Extruding AIF Region ' + Region->Name(), i/(d[2]-1E)

		Bin = 0B
		for j=0L,dR[3]-1 do Bin = Bin or Region->Read(Path,i,j)
		if total(Bin) gt 0 then for j=r[0],r[1] do Extrude->Write, Path, bin, i, j
	endfor


	PMI__Control, ev.top, /refresh
end



pro PMI__Button__Control__iBEAt_DCE_AIF_Segmentation, id, v

	PMI__Info, tlb(id), Stdy=Stdy
	if obj_valid(Stdy) then begin
		Series = Stdy->Names(0,ns,DefDim=3)
		sensitive = ns gt 0
	endif else sensitive=0
    widget_control, id, sensitive=sensitive
end

function PMI__Button__iBEAt_DCE_AIF_Segmentation, parent,value=value, separator=separator

	if n_elements(value) eq 0 then value = 'iBEAt Semi-quantitative parameters (Pixel)'

    id = widget_button(parent $
    , 	value 		= value $
	, 	event_pro	= 'PMI__Button__Event__iBEAt_DCE_AIF_Segmentation' $
	,	pro_set_value 	= 'PMI__Button__Control__iBEAt_DCE_AIF_Segmentation' $
	, 	separator 	= separator	)

	return, id
end

