; docformat = 'rst'
;
; NAME:
;    GETINDEXRANGE
;
; PURPOSE:
;+
;   Given a set of monotonically increasing data and a vector in the form 
;   [minimum value, maximum value] this program calculates the index range over which 
;   the min and max values span.
;
; :Categories:
;    Index Utility
;
; :Examples:
;    IDL> density_asym
;
; :Params:
;       DATA:                   in, required, type=any
;                               A vector of monotonically increasing values.
;       RANGE:                  in, required, type=string
;                               The minimum and maximum values of `DATA` for which the
;                                   index range is to be returned.
;
; :Returns:
;       IRANGE:                 The index range within `DATA` corresponding to the values
;                                   of `RANGE`. If the matches are not exact, DATA[IRANGE]
;                                   lie just inside of RANGE.
;
; :Author:
;    Matthew Argall::
;    University of New Hampshire
;    Morse Hall Room 113
;    8 College Road
;    Durham, NH 03824
;    matthew.argall@wildcats.unh.edu
;
; :History:
;    Modification History::
;       05/06/2013  -   Written by Matthew Argall
;       2013-10-25  -   Order `IRANGE` [min, max]. Was rounding up when range[0] > max(data).
;                           Round up if `DATA` is negative. - MRA
;       2014/01/26  -   Typo to check of RANGE is inside DATA resulted bumping exact
;                           values and leaving rounded values as is. Fixed. - MRA
;-
function getIndexRange, data, range
    compile_opt strictarr
    on_error, 2
    
    ;Locate the index values of RANGE within DATA
    irange = value_locate(data, range)
    
    ;If RANGE is smaller than DATA[0], then take index 0
    if irange[0] eq -1 then irange[0] = 0
    if irange[1] eq -1 then irange[1] = 0
    
    ;Order IRANGE as [min, max]
    irange = irange[0] lt irange[1] ? irange : reverse(irange)

    ;IRANGE[0] must index a value that is >= RANGE[0]
    irange[0] = data[irange[0]] gt range[0] ? irange[0] : irange[0]+1
    
    ;IRANGE[1] must index a value that is <= RANGE[1]
    irange[1] = data[irange[1]] lt range[1] ? irange[1] : irange[1]-1
    
    return, irange
end