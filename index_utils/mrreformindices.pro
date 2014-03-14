; docformat = 'rst'
;
; NAME:
;       MrReformIndices
;
;*****************************************************************************************
;   Copyright (c) 2014, Matthew Argall                                                   ;
;   All rights reserved.                                                                 ;
;                                                                                        ;
;   Redistribution and use in source and binary forms, with or without modification,     ;
;   are permitted provided that the following conditions are met:                        ;
;                                                                                        ;
;       * Redistributions of source code must retain the above copyright notice,         ;
;         this list of conditions and the following disclaimer.                          ;
;       * Redistributions in binary form must reproduce the above copyright notice,      ;
;         this list of conditions and the following disclaimer in the documentation      ;
;         and/or other materials provided with the distribution.                         ;
;       * Neither the name of the <ORGANIZATION> nor the names of its contributors may   ;
;         be used to endorse or promote products derived from this software without      ;
;         specific prior written permission.                                             ;
;                                                                                        ;
;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY  ;
;   EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES ;
;   OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT  ;
;   SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,       ;
;   INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED ;
;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR   ;
;   BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN     ;
;   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN   ;
;   ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  ;
;   DAMAGE.                                                                              ;
;*****************************************************************************************
;
; PURPOSE
;+
;   The purpose of this function is to convert subscript ranges to a 1D array of
;   subscripts.
;
; :Examples:
;   See the main level program at the end of this routine::
;       IDL> .r MrReformIndices
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
;	Modification History::
;       2013/03/02  -   Written by Matthew Argall.
;-
;*****************************************************************************************
;+
;   The purpose of this function is to convert a range of subscripts to an array
;   of subscripts.
;
; :Params:
;       DIM:                in, required, type=lonarr
;                           Size of each dimension of the original array.
;       ISRANGE:            in, required, type=intarr
;                           A vector that has one element for each Subscript argument
;                               supplied by the user; each element contains a zero if the
;                               corresponding input argument was a scalar index value or
;                               array of indices, or a one if the corresponding input
;                               argument was a subscript range.
;       SUBSCRIPT:          in, required, type=integer/intarr(3)
;                           Index subscripts. Either a scalar, an index array, or a 
;                               subscript range in the form [start, stop, step_size]
;
; :Keywords:
;       COUNT:              out, optional, type=long
;                           The number of indices.
;
; :Returns:
;       RESULT:             Index array spanned by SUBSCRIPT.
;-
function MrReformIndices_Subscripts, dim, isRange, subscript, $
COUNT=nInds
    compile_opt strictarr

    ;Specific indices were given?
    if isRange eq 0 then begin
        result = subscript
        nInds = n_elements(subscript)
    
    ;Range of indices
    endif else begin
        ;Convert to positive indices
        sub = subscript
        if sub[0] lt 0 then sub[0] = dim - sub[0]
        if sub[1] lt 0 then sub[1] = dim - sub[1]
        
        ;Create the array of indices
        nInds = ((sub[1] - sub[0] + 1) / sub[2]) > 1
        result = sub[0] + sub[2]*lindgen(nInds)
    endelse
    
    return, result
end


;+
;
; :Params:
;       DIMS:               in, required, type=lonarr
;                           Size of each dimension of the original array.
;       ISRANGE:            in, required, type=intarr
;                           A vector that has one element for each Subscript argument
;                               supplied by the user; each element contains a zero if the
;                               corresponding input argument was a scalar index value or
;                               array of indices, or a one if the corresponding input
;                               argument was a subscript range.
;       SUBSCRIPT1:         in, required, type=integer/intarr(3)
;                           Index subscripts. Either a scalar, an index array, or a 
;                               subscript range in the form [start, stop, step_size]
;       SUBSCRIPT2:         in, optional, type=integer/intarr(3)
;                           Index subscripts.
;       SUBSCRIPT3:         in, optional, type=integer/intarr(3)
;                           Index subscripts.
;       SUBSCRIPT4:         in, optional, type=integer/intarr(3)
;                           Index subscripts.
;       SUBSCRIPT5:         in, optional, type=integer/intarr(3)
;                           Index subscripts.
;       SUBSCRIPT6:         in, optional, type=integer/intarr(3)
;                           Index subscripts.
;       SUBSCRIPT7:         in, optional, type=integer/intarr(3)
;                           Index subscripts.
;       SUBSCRIPT8:         in, optional, type=integer/intarr(3)
;                           Index subscripts.
;
; :Keywords:
;       COUNT:              out, optional, type=long
;                           The number of indices.
;
; :Returns:
;       RESULT:             Array of 1-Dimensional subscripts corresponding to the input
;                               subscript ranges.
;-
function MrReformIndices, dims, isRange, subscript1, subscript2, subscript3, subscript4, $
                                         subscript5, subscript6, subscript7, subscript8, $
COUNT=count, $
SORT=sort
    compile_opt strictarr
    on_error, 2

    ;Number of subscripts given
    nSubs = n_elements(isRange)

    ;First indicex of each dimension
    dimProduct = product(dims, /CUMULATIVE, /INTEGER)
    
    ;
    ; Indices are generated by
    ;
    ;       index = (# elements in previous dimension) * (index in current dimension)
    ;
    ; and summing over each dimension.
    ;
    
    ;Step through each subscript
    count = 0L
    for i = nSubs-1, 0, -1 do begin
    
        ;Convert subscript ranges to arrays of indices.
        case i of
            7: tempInds = MrReformIndices_Subscripts(dims[i], isRange[i], subscript8, COUNT=n)
            6: tempInds = MrReformIndices_Subscripts(dims[i], isRange[i], subscript7, COUNT=n)
            5: tempInds = MrReformIndices_Subscripts(dims[i], isRange[i], subscript6, COUNT=n)
            4: tempInds = MrReformIndices_Subscripts(dims[i], isRange[i], subscript5, COUNT=n)
            3: tempInds = MrReformIndices_Subscripts(dims[i], isRange[i], subscript4, COUNT=n)
            2: tempInds = MrReformIndices_Subscripts(dims[i], isRange[i], subscript3, COUNT=n)
            1: tempInds = MrReformIndices_Subscripts(dims[i], isRange[i], subscript2, COUNT=n)
            0: tempInds = MrReformIndices_Subscripts(dims[i], isRange[i], subscript1, COUNT=n)
        endcase
        
        ;We need an array
        if n eq 1 then tempInds = [tempInds]
        
        ;Last time through
        if i eq 0 then begin
            if nSubs eq 1 then begin
                indices = temporary(tempInds)
                count = n
            endif else begin
                indices = transpose(rebin(temporary(tempInds), n, count)) + rebin(indices, count, n)
                indices = reform(indices, count*n)
            endelse
            
        ;First time through
        endif else if i eq nSubs-1 then begin
            indices = dimProduct[i-1] * temporary(tempInds)
            if n eq 1 then indices = [indices]
            count = n
        
        ;Make sure we add the array of indices to each index already generated.
        endif else begin
            tempInds *= dimProduct[i-1]
            indices = transpose(rebin(temporary(tempInds), n, count)) + rebin(indices, count, n)
            count *= n
            indices = reform(indices, count)
        endelse
    endfor
    
    ;Sort the indices into ascending order?
    if keyword_set(sort) then indices = indices[sort(indices)]
    
    return, indices
end

    
;---------------------------------------------------------------------
;Main-Level Program (IDL> .r MrReformIndices) ////////////////////////
;---------------------------------------------------------------------

;EXAMPLE 1
array = indgen(2,4,4,2)
dims = size(array, /DIMENSIONS)

indices = MrReformIndices(dims, [0,0,1,0], 0, [0,2], [1,3,1], 1, /SORT, COUNT=n)

print, '--------------------------------------------------------'
help, array
print, 'array[0, [0, 2], 1:3, 1]  = [' + strjoin(strtrim(reform(array[0, [0, 2], 1:3, 1], n), 2), ', ') + ']'
print, 'array[indices]            = [' + strjoin(strtrim(array[indices], 2), ', ') + ']'
print, ''

;EXAMPLE 2
array = indgen(6)
dims = size(array, /DIMENSIONS)
indices = MrReformIndices(dims, 0, [0,3,5], /SORT, COUNT=n)

print, '--------------------------------------------------------'
help, array
print, 'array[[0,3,5]]            = [' + strjoin(strtrim(reform(array[[0,3,5]], n), 2), ', ') + ']'
print, 'array[indices]            = [' + strjoin(strtrim(array[indices], 2), ', ') + ']'
print, ''
end

