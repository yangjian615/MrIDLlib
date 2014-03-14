function time_select, t, tstart, tend, $
;KEYWORDS
  SUBSET=subset, $
  INTERVAL=interval
  
;--------------------------------------------------------------------------------
; written by Matthew Argall 09/07/2011
;
; TIME_SELECT extracts the given time interval [tstart,tend] from a 1D time array.
;
; More generally, this can select any subset of data where tstart is the minimum
; desired value and tmax is the maximum desired value. However, the overall maximum
; and miminum values have to be located at the enpoints of the 1D array. This latter
; restriction can be fixed easily. 
;
; PARAMETERS
;   t - a 1D monotonically increasing array of time data in seconds from the beginning of the day.
;   tstart - the starting time of the desired interval with same units as t
;   tend - the end time of the desired interval with same units as t
;
; KEYWORDS
;   subset - returns the indices of t which match (tstart le t le  tend)
;   interval - an error catching keyword
;       0 - the interval provided [tstart,tend] is a valid interval and 
;       1 - the itnerval [tstart,tend] is outside of the the array t
;--------------------------------------------------------------------------------

  npts = n_elements(t)
  if tstart lt t[npts-1] and tend gt t[0] then begin
    interval = 0
    subset = where(t ge tstart and t le tend, npts)
    t_sub = t(subset)
  endif else begin
    interval = 1
    tstart = t[0]
    tend = t[npts-1]

    subset = lindgen(npts)
    t_sub = t(subset)
    print, '*****TIME INTERVAL NOT VALID. SELECTING ALL AVAILABLE DATA*****'
  endelse
  
return, t_sub

end