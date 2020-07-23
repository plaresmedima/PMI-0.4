FUNCTION iBEAt_Import__LoadSequence, Stdy, Series, files, status, SORTED_FILES=files_sort

  PMI__Message, status, 'Sorting ' + Series

  z = PMI__Dicom__Read(files,'0020'x,'1041'x)
  t = PMI__Dicom__Read(files,'0008'x,'0032'x)
  files_sort = PMI__Dicom__Sort(files, z, t)

  nx = PMI__Dicom__Read(files_sort,'0028'x,'0011'x)
  ny = PMI__Dicom__Read(files_sort,'0028'x,'0010'x)

  d = [max(nx),max(ny),n_elements(z),n_elements(t)]
  Dcm = Stdy -> New('SERIES', Name = Series, Domain = {z:z, t:t, m:d[0:1]})

  x = (d[0]-nx)/2
  y = (d[1]-ny)/2

  im = fltarr(d[0],d[1])
  range = [0E,0E]
  for k=0L,d[2]*d[3]-1 do begin

	PMI__Message, status, 'Loading ' + Series, k/(d[2]*d[3]-1.0)

	image = PMI__Dicom__ReadImage(files_sort[k])
	if size(image,/n_dimensions) ne 0 then begin
		im[x[k]:x[k]+nx[k]-1,y[k]:y[k]+ny[k]-1] = image
		range[0] = min([range[0],min(image,max=max)])
		range[1] = max([range[1],max])
	endif
	Dcm -> Write, Stdy->DataPath(), im, k
	im *= 0

  endfor

  Dcm -> Trim, [range[0],0.8*range[1]]
  Dcm -> ReadDicom, files_sort[0]

  RETURN, Dcm

end