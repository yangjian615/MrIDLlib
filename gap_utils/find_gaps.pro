; docformat = 'rst'
;
;+
;   The purpose of this program is to find data gaps within a monotonically increasing
;   time array.
;
; :Categories:
;   Gap Utility
;
; :Params:
;       T:                  in, required, type=real numeric
;                           A monotonically increasing time array in which to look for
;                               gaps.
;       DT_MIN:             in, required, type=same as `T`
;                           The minimum size of a data gap in the same units as `T`.
;       DT_MAX:             in, required, type=same as `T`
;                           The maximum size of a data gap in the same units as `T`.
;
; :Keywords:
;       NGAPS:              out, optional, type=int
;                           The number of gaps found
;
; :Returns:
;       GAPS:               The index location into `T` that marks the last point in
;                               a continuous data interval. If `NGAPS`=0, then !NULL is
;                               returned.
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :History:
;   Modification History::
;       11/26/2012  -   Written by Matthew Argall
;-
function find_gaps, t, dt_min, dt_max, $
NGAPS = ngaps
	compile_opt idl2
	
	if n_params() eq 0 then begin
	    n_min = 1
	    n_max = !values.f_infinity
	endif
	
	;Calculate the amount of time between samples. 
	dt = t[1:*] - t[0:-2]
	
	;Find gaps 
	gaps = where(dt gt dt_min and dt lt dt_max, ngaps)
	if ngaps eq 0 then gaps = !Null
	
	return, gaps
end
	
	