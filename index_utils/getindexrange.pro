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
;   If the range given falls between two adjacent data points, the resulting index range
;   will indicate a single point in DATA less than RANGE[0].
;
; :Categories:
;    Index Utility
;
; :Examples:
;    IDL> density_asym
;
; :Params:
;       DATA:                   in, required, type=any
;                               A vector of monotonically increasing or decreasing values.
;       RANGE:                  in, required, type=integer/intarr(2\,N)
;                               The minimum and maximum values of `DATA` for which the
;                                   index range is to be returned, where RANGE[0,*] is the
;                                   minimum range in `DATA` and RANGE[1,*] is the maximum
;                                   range in `DATA` (or vice versa). All [min,max] range
;                                   pairs must be strictly ascending or descending.
;       ERANGE:                 in, optional, type=integer/intarr
;                               A scalar or vector indicating the maximum (minimum) range
;                                   in `DATA`. If given, `RANGE` must also be a scalar or
;                                   vector and indicates the minimum (maximum) range in
;                                   `DATA`. 
;
; :Keywords:
;       STRIDE:                 out, optional, type=integer
;                               If one is indexing from `IRANGE`[0] to `IRANGE`[1], then
;                                   STRIDE indicates whether the stride should increase
;                                   or decrease. E.g.::
;                                       output = data[range[0]:range[1]:stride]
;
; :Returns:
;       IRANGE:                 A 2xN array giving the index range within `DATA`
;                                   corresponding to the values of `RANGE`. If the matches
;                                   are not exact, DATA[IRANGE] lie just inside of RANGE.
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
;       2014/05/22  -   Typo caused ranges to be bumped when exact match was found. Fixed. - MRA
;       2014/06/02  -   Both DATA and RANGE can be in descending order. - MRA
;       2014/06/03  -   Added the STRIDE keyword. Indexing problems were occurring when
;                           RANGE fell between two adjacent points in DATA. Fixed. - MRA
;       2014/06/11  -   Added the ERANGE parameter. More than one [min,max] range pair
;                           are accepted. - MRA
;       2014/07/19  -   Typo was changing data values instead of index values. Fixed. - MRA
;-
function getIndexRange, data, range, eRange, $
STRIDE=stride
    compile_opt strictarr
    on_error, 2
    
    ;Create a Nx2 array.
    ;   - work with col-major array
    if n_elements(eRange) gt 0 then begin
        _range = [[range], [eRange]]
    endif else begin
        dims = size(range, /DIMENSIONS)
        if dims[0] ne 2 then message, 'RANGE must be a 2xN array.'
        _range = transpose(range)
    endelse

    ;Descending order?
    ascending =  data[1]    gt  data[0]    ? 1 : 0
    highLow   = _range[0,0] gt _range[0,1] ? 1 : 0
    
    ;Stride
    ;   - If highLow and ascending are the same, then STRIDE=-1
    if (highLow and ascending) || (highLow eq 0 && ascending eq 0) $
        then stride = -1 $
        else stride = 1
    
    ;Locate the index values of RANGE within DATA
    iRange = value_locate(data, _range)

    ;If RANGE is smaller than DATA[0], then take index 0
    iMinus = where(iRange eq -1, nMinus)
    if nMinus gt 0 then iRange[iMinus] = 0
    
    ;If the indices are the same, then they fall between two adjacent points in DATA.
    iDiff = where(iRange[*,0] ne iRange[*,1], nDiff)
    if nDiff eq 0 then return, iRange

    ;Ascending data?
    if ascending then begin
        ;Descending range?
        if highLow then begin
            ;Endpoints greater than the maximum range desired?
            iGT = where(data[iRange[iDiff,0]] gt _range[iDiff,0], nGT)
            if nGT gt 0 then data[iRange[iDiff[iGT],0]] = iRange[iDiff[iGT],0]-1
            
            ;Endpoints less than than the minimum range desired?
            iLT = where(data[iRange[iDiff,1]] lt _range[iDiff,0], nLT)
            if nLT gt 0 then iRange[iDiff[iLT],1] = iRange[iDiff[iLT],1]+1
            
        ;Ascening range?
        endif else begin
            ;Endpoints less than than the minimum range desired?
            iLT = where(data[iRange[iDiff,0]] lt _range[iDiff,0], nLT)
            if nLT gt 0 then iRange[iDiff[iLT],0] = iRange[iDiff[iLT],0]+1
            
            ;Endpoints greater than the maximum range desired?
            iGT = where(data[iRange[iDiff,1]] gt _range[iDiff,1], nGT)
            if nGT gt 0 then iRange[iDiff[iGT],1] = iRange[iDiff[iGT],1]-1
        endelse

    ;Descending data?
    endif else begin
        ;Descending range?
        if highLow then begin
            ;Endpoints greater than the maximum range desired?
            iGT = where(data[iRange[iDiff,0]] gt _range[iDiff,0], nGT)
            if nGT gt 0 then iRange[iDiff[iGT],0] = iRange[iDiff[iGT],0]+1
            
            ;Endpoints less than than the minimum range desired?
            iLT = where(data[iRange[iDiff,1]] lt _range[iDiff,0], nLT)
            if nLT gt 0 then iRange[iDiff[iLT],1] = iRange[iDiff[iLT],1]-1
        
        ;Ascending range?
        endif else begin
            ;Endpoints less than than the minimum range desired?
            iLT = where(data[iRange[iDiff,0]] lt _range[iDiff,0], nLT)
            if nLT gt 0 then iRange[iDiff[iLT],0] = iRange[iDiff[iLT],0]-1
            
            ;Endpoints greater than the maximum range desired?
            iGT = where(data[iRange[iDiff,1]] gt _range[iDiff,1], nGT)
            if nGT gt 0 then iRange[iDiff[iGT],1] = iRange[iDiff[iGT],1]+1
        endelse
    endelse
    
    ;Transpose the result to return a 2xN array
    return, transpose(iRange)
end