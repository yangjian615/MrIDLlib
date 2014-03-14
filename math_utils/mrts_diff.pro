; docformat = 'rst'
;
; NAME:
;       MrTS_Diff.PRO
;
; PURPOSE:
;+
;   The is a substitude for the TS_Diff function. Differences include::
;       - TS_Diff is not truly recursive -- it has a FOR loop. This program is.
;       - Compute the backward difference
;       - Set RECURSIVE=0 to compute a shifted difference
;       - Input type is preserved unless DOUBLE is set.
;
; :Categories:
;   Statistics
;
; :Params:
;
;       DATA:               in, required, type=any
;                           The data to be differenced.
;       ORDER:              in, optional, type=int, default=1
;                           The number of points to look ahead when differencing.
;       NR:                 in, optional, hidden, type=integer
;                           Number of recursive steps remaining. This parameter is for
;                               internal use only.
;
; :Keywords:
;       DIMENSION:          in, optional, tyep=int, default=1
;                           The dimension over which to difference `DATA`.
;       DOUBLE:             in, optional, type=int, default=0
;                           Perform the differencing with double precision.
;       RECURSIVE:          in, optional, type=boolean, default=0
;                           Instead of looking ahead `ORDER` number of points to take the
;                               differnece, take the 1-st order difference `ORDER` number
;                               of times. Setting both `ORDER`>0 and `RECURSIVE` produces
;                               the same results as calling IDL's TS_DIFF routine.
;
; :Returns:
;       DATA_DIFF:          out, required, type=Size(`DATA`\, /TYPE)
;                           The differenced data.
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
;       10/01/2013  -   Written by Matthew Argall
;-
function MrTS_Diff, data, order, nr, $
DIMENSION = dimension, $
DOUBLE = double, $
RECURSIVE = recursive
    compile_opt idl2
    
;---------------------------------------------------------------------
;Check Inputs ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Defaults
    if n_elements(order)     eq 0 then order = -1
    if n_elements(dimension) eq 0 then dimension = 0
    if n_elements(recursive) eq 0 then recursive = 1 else recursive = keyword_set(recursive)
    if n_elements(nr)        eq 0 then nr = 0
    if n_elements(double)    eq 0 then double = 0
    
    ;We do not need to recurse if ORDER = +/-1
    if abs(order) eq 1 then recursive = 0
    
    ;If RECURSIVE is set, then ORDER represents the number of recursions to perform.
    ;A +/-1-point difference will be taken each time. Note that while recursing, NR > 0
    ;and RECURSIVE = 0.
    if (recursive eq 1) then begin
        nr = abs(order)
        order = order / abs(order)
    endif
    
    ;DATA must have two or fewer dimensions.
    ndims = size(data, /N_DIMENSIONS)
    if ndims gt 2 then message, 'DATA must have 1 or 2 dimensions.'
    
    ;Sizes of each dimension of DATA
    dims = size(data, /DIMENSIONS)
    if (double eq 1) $
        then data_diff = make_array(dims, /DOUBLE) $
        else data_diff = make_array(dims, TYPE=size(data, /TYPE))
    
    ;Number of points available for differencing
    if dimension eq 0 then nPts = n_elements(data) else nPts = dims[dimension-1]
    if order gt nPts then message, 'Order must be less than ' + strtrim(nPts-order, 2)
    
;---------------------------------------------------------------------
;Compute Difference //////////////////////////////////////////////////
;---------------------------------------------------------------------

    ;Backward differencing
    if order lt 0 then begin
        case dimension of
            0: data_diff[0:nPts-1-order]   = data[  order:nPts-1]   - data[  0:nPts-order]
            1: data_diff[0:nPts-1-order,*] = data[  order:nPts-1,*] - data[  0:nPts-order,*]
            2: data_diff[*,0:nPts-1-order] = data[*,order:nPts-1]   - data[*,0:nPts-order]
        endcase
    
    ;Forward differencing
    endif else if order gt 0 then begin
        case dimension of
            0: data_diff[0:nPts-1-order]   = data[  0:nPts-order]   - data[  order:nPts-1]
            1: data_diff[0:nPts-1-order,*] = data[  0:nPts-order,*] - data[  order:nPts-1,*]
            2: data_diff[*,0:nPts-1-order] = data[*,0:nPts-order]   - data[*,order:nPts-1]
        endcase
        
    endif else message, 'ORDER cannot be 0.'

;---------------------------------------------------------------------
;Recursive Differencing //////////////////////////////////////////////
;---------------------------------------------------------------------

    nr -= 1
    if nr gt 0 then begin
        ;Compute the difference again. Note that while recursing, NR > 0
        ;and RECURSIVE = 0.
        data_diff = MrTS_Diff(temporary(data_diff), order, nr, DIMENSION=dimension, DOUBLE=double, RECURSIVE=0)
        
        ;Return ORDER to its original value
        order += 1
    endif

    ;Set the last ORDER elements equal to zero. This is only necessry if we were recursing.
    ;If we are not recursing, by the time we get here, NR=-1, so the IF statement will not
    ;be executed.
    if nr eq 0 then begin
        case dimension of
            0: data_diff[nPts-order:nPts-1]   = 0
            1: data_diff[nPts-order:nPts-1,*] = 0
            2: data_diff[*,nPts-order:nPts-1] = 0
        endcase
    endif
    
    return, data_diff
end