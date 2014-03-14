; docformat = 'rst'
;
; NAME:
;       LINSPACE
;
; PURPOSE:
;+
;       The purpose of this program is to create an array equally spaced points.
;
; :Categories:
;       Array Utility
;
; :Params:
;
;       A               -   In. Required. Type=float.
;                           First number in the array.
;       B               -   In. Required. Type=float.
;                           Last number in the array.
;       N               -   In. Required. Type=Int.
;                           Number of points between A and B.
;
; :Keywords:
;
;       INTERVAL        -   in, out, optional, type=Boolean, default=0
;                           If set, `N` represents the spacing of points between `A`
;                               and `B` (not the number of points between them). If not
;                               set, then a named variable into which the interval between
;                               points can be returned.
;       TYPE            -   in, optional, type=Boolean, default=4 (float)
;                           If `INTERVAL` is set, then this is the data-type of the
;                               returned array.
;                           
; :Returns:
;
;       L               -   Array of N points spaced evenly between A and B.
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
;       Written by:     Matthew Argall 29 November 2012
;       02/12/2013  -   Added INTERVAL and TYPE keywords, prevent divide by zero when
;                           n=1 - MRA
;       08/04/2013  -   When INTERVAL is not provided, it can optionally be returned. - MRA
;       11/26/2013  -   INTERVAL=1 and A > B caused negative array index. Fixed. - MRA
;-
function linspace, a, b, n, $
INTERVAL = interval, $
TYPE = type
    compile_opt strictarr
    on_error, 2

    ;n represents the spacing between points
    if keyword_set(interval) then begin 
        if n_elements(type) eq 0 then type = 4

        n_intervals = floor((b - a) / n)
        if n_intervals lt 0 $
            then l = a - n*make_array(abs(n_intervals)+1, /INDEX, TYPE=type) $
            else l = a + n*make_array(n_intervals+1, /INDEX, TYPE=type)
        
    ;n is the number of points between a and b
    endif else begin

        ;If only 1 point is desired, return "a"
        if n eq 1 then return, a
    
        l = dindgen(n) / (n - 1D) * (b - a) + a
        interval = l[1] - l[0]
    endelse
    
	return, l
end

