function satellite_separation, positions, $;s1, s2
;KEYWORDS
DISTANCE = distance
;---------------------------------------------------------------------
;Created by Matthew Argall 11/22/2011
;
;SATELLITE_SEPARATION.PRO calculates the three component separation
;between two spacecraft as well as the total distance between
;them. The separation vector points from s1 to s2.
;
;INPUTS
; s1 - the position of satellite 1
; s2 - the position of satellite 2
;
;KEYWORDS
; DISTANCE - set this keyword to a named variable if the total
;            distance is to be returned.
;
;OUTPUTS
; separation - the three component separation vector that points from
;              s1 to s2
;
;---------------------------------------------------------------------

  ;duplicate each position vector 3 times to make them 3x3 matrices
  s1 = rebin(positions[*,0], 3, 4)
  s2 = rebin(positions[*,1], 3, 4)
  s3 = rebin(positions[*,2], 3, 4)
  s4 = rebin(positions[*,3], 3, 4)

  ;then, just subtract the positions matrix from the individual
  ;satellite matrix to gat the separation vector from one spacecraft
  ;to the others
  separation = fltarr(3, 4, 4)
  separation[*,*,0] = positions - s1
  separation[*,*,1] = positions - s2
  separation[*,*,2] = positions - s3
  separation[*,*,3] = positions - s4

  ;compute the total distance from one spacecraft to the others
  ;this will be a 4x4 antisymmetric matrix
  distance = fltarr(4,4)
  for i = 0, 3 do begin
      for j = 0, 3 do begin
          distance[i,j] = total(separation[*,i,j]^2)
      endfor
  endfor

  return, separation

end
