PRO split_into_six_segments, bin, p1, seg1=bin1, seg2=bin2, seg3=bin3, seg4=bin4, seg5=bin5, seg6=bin6

  bin1 = bin*0B
  bin2 = bin*0B
  bin3 = bin*0B
  bin4 = bin*0B
  bin5 = bin*0B
  bin6 = bin*0B

  dim = size(bin,/dimensions)

  c = cos(90*!PI/180)
  s = sin(90*!PI/180)

  p0 = Centroid(bin)
  vx = p1-p0
  vy = [c*vx[0]-s*vx[1], s*vx[0]+c*vx[1]]

  for x=0L, dim[0]-1 do begin
  for y=0L, dim[1]-1 do begin
    if bin[x,y] eq 1 then begin
      v = [x,y] - p0
      ax = acos(total(v*vx)/(norm(v)*norm(vx)))*180/!PI
      ay = acos(total(v*vy)/(norm(v)*norm(vy)))*180/!PI
      if ay ge 90 then ax = 360-ax
      if ax ge 300 then bin1[x,y]=1 else $
      if ax ge 240 then bin6[x,y]=1 else $
      if ax ge 180 then bin5[x,y]=1 else $
      if ax ge 120 then bin4[x,y]=1 else $
      if ax ge 60 then bin3[x,y]=1 else $
      if ax ge 0 then bin2[x,y]=1
    endif
  endfor
  endfor

END