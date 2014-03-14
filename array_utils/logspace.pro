; docformat = 'rst'
;
; NAME:
;
;       LOGSPACE
;
; PURPOSE:
;+
;       The purpose of this program is to create an array equally-, logarithmically-spaced
;       points. They will show up as a straight line on a log-scale plot.
;
; :Categories:
;
;       Array Utility
;
; :Params:
;
;       A:                  in, required, type=float
;                           The power of ten at which to begin creating the array.
;       B:                  in, required, type=float
;                           The power of ten of the last point in the array.
;       N:                  in, required, type=float
;                           Number of points between A and B.
;                           
; :Returns:
;
;       LOGARR:             Array of N points logarithmically spaced between A and B.
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
;-
function logspace, a, b, n
    compile_opt idl2
    
    powers_of_ten = dindgen(n) / (n - 1D) * (b - a) + a
    logarr = 10.0^powers_of_ten
    
    return, logarr
end